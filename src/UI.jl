module UI

using Util
using Data
using Query
using Flows
using Match
using HttpServer
using WebSockets
using JSON

# --- parsing ---

const Splice = Vector{Union{String, Symbol}} 

struct AttributeNode
  key::Splice
  val::Splice
end

@enum FixedNodeKind Text Html

struct FixedNode
  content::Splice
  kind::FixedNodeKind
end

struct QueryNode
  table::Symbol
  vars::Vector{Symbol}
end

const Node = Union{AttributeNode, FixedNode, QueryNode}
  
struct Parsed
  node::Vector{Node} # in pre-order
  parent::Vector{Int64} # parent[1] = 0, arbitrarily
end

function parse_value(expr)
  convert(Splice, @match expr begin
    _::String => [expr]
    _::Symbol => [string(expr)]
    Expr(:string, args, _) => args
    _ => error("What are this? $expr")
  end)
end

function parse(expr)
  node = Vector{Node}()
  parent = Vector{Int64}()
  
  parse_queue = Vector{Tuple{Int64, Any}}()
  push!(parse_queue, (0, expr))
  while !isempty(parse_queue)
    (my_parent, expr) = shift!(parse_queue)
    @match expr begin
      Expr(:line, _, _) => nothing
      Expr(:block, [Expr(:line, _, _), expr], _) => begin
        push!(parse_queue, (my_parent, expr))
      end
      Expr(:vect || :vcat || :hcat, exprs, _) => begin 
        push!(parse_queue, (my_parent, exprs))
      end
      Expr(:call, [table::Symbol, Expr(:->, [Expr(:tuple, [], _), Expr(:block, exprs, _)], _), vars...], _) => begin
        push!(node, QueryNode(table, vars))
        push!(parent, my_parent)
        for expr in exprs
          push!(parse_queue, (length(node), expr))
        end
      end
      [tag, exprs...] => begin
        push!(node, FixedNode(parse_value(tag), Html))
        push!(parent, my_parent)
        for expr in exprs
          push!(parse_queue, (length(node), expr))
        end
      end
      Expr(:(=), [key, val], _) => begin
        push!(node, AttributeNode(parse_value(key), parse_value(val)))
        push!(parent, my_parent)
      end
      other => begin
        push!(node, FixedNode(parse_value(other), Text))
        push!(parent, my_parent)
      end
    end
  end

  Parsed(node, parent)
end

# --- compiling ---

default{T <: Number}(::Type{T}) = zero(T)
default(::Type{String}) = ""

struct Compiled
  flow::Flow
  group_ids::Vector{Symbol}
end

function compile(node, parent, column_type::Function)
  @assert node[1] isa FixedNode 
  @assert node[1].content == ["html"]
  
  fixed_parent = Dict{Int64, Int64}(1 => 0)
  query_parent = Dict{Int64, Int64}(1 => 0)
  for id in 2:length(node) # node 1 has no real parents
    my_parent = parent[id]
    fixed_parent[id] = fixed_parent[my_parent]
    query_parent[id] = query_parent[my_parent]
    @match node[my_parent] begin
      _::QueryNode => query_parent[id] = my_parent
      _::FixedNode => fixed_parent[id] = my_parent
    end
  end
  
  vars = Dict{Int64, Vector{Symbol}}(0 => [:session])
  types = Dict{Int64, Vector{Type}}(0 => [String])
  free_vars = Dict{Int64, Vector{Symbol}}(0 => [])
  free_types = Dict{Int64, Vector{Type}}(0 => [])
  for (id, my_node) in enumerate(node)
    my_vars = vars[id] = copy(vars[parent[id]])
    my_types = types[id] = copy(types[parent[id]])
    my_free_vars = free_vars[id] = Vector{Symbol}()
    my_free_types = free_types[id] = Vector{Type}()
    if my_node isa QueryNode
      for (ix, var) in enumerate(my_node.vars)
        if (var != :(_)) && !(var in my_vars)
          push!(my_vars, var)
          push!(my_types, column_type(my_node.table, ix))
          push!(my_free_vars, var)
          push!(my_free_types, column_type(my_node.table, ix))
        end
      end
    end
  end
  
  family = Dict{Int64, Vector{Int64}}(id => [id] for (_, id) in fixed_parent)
  for (id, my_node) in enumerate(node)
    if !(my_node isa AttributeNode)
      push!(family[fixed_parent[id]], id)
    end
  end
  
  num_children = Dict{Int64, Int64}(id => 0 for id in 0:length(node))
  lineage = Dict{Tuple{Int64, Int64}, Int64}()
  # lineage[hi, lo] = n iff lo is the nth child of hi or a descendant thereof
  # lineage[hi, lo] = 0 otherwise
  for (_, my_family) in family
    for hi_id in my_family
      for lo_id in my_family
        if hi_id >= lo_id # note ids are numbered depth-first
          lineage[hi_id, lo_id] = 0
        elseif hi_id == parent[lo_id]
          lineage[hi_id, lo_id] = (num_children[hi_id] += 1)
        else
          lineage[hi_id, lo_id] = lineage[hi_id, parent[lo_id]]
        end
      end
    end
  end
  
  const KeyElem = Union{Int64, Type, Tuple{Symbol, Type}}
  key = Dict{Int64, Vector{KeyElem}}(0 => KeyElem[(:session, String)])
  for (my_fixed_parent, my_family) in family
    base_key = Vector{KeyElem}()
    append!(base_key, zip(vars[my_fixed_parent], types[my_fixed_parent]))
    for lo_id in my_family[2:end] # don't include parent
      if node[lo_id] isa FixedNode
        @assert !haskey(key, lo_id)
        my_key = key[lo_id] = copy(base_key)
        for hi_id in my_family
          if (hi_id == my_fixed_parent) || (node[hi_id] isa QueryNode) 
            if lineage[hi_id, lo_id] == 0
              append!(my_key, free_types[hi_id])
              push!(my_key, 0)
            else
              append!(my_key, zip(free_vars[hi_id], free_types[hi_id]))
              push!(my_key, lineage[hi_id, lo_id])
            end
          end
        end
      end
    end
  end
  
  key_vars = Dict{Int64, Vector{Any}}()
  key_exprs = Dict{Int64, Vector{Any}}()
  key_types = Dict{Int64, Vector{Type}}()
  for (id, my_key) in key
    my_key_vars = key_vars[id] = Vector{Any}()
    my_key_exprs = key_exprs[id] = Vector{Any}()
    my_key_types = key_types[id] = Vector{Type}()
    for key_elem in my_key
      (var, expr, typ) = @match key_elem begin
        _::Int64 => (key_elem, key_elem, Int64)
        _::Type => (:(_), :(default($key_elem)), key_elem)
        (var, typ) => (var, var, typ)
      end
      push!(my_key_vars, var)
      push!(my_key_exprs, expr)
      push!(my_key_types, typ)
    end
  end
  
  creates = Vector{Create}()
  merges = Vector{Merge}()
  group_ids = Vector{Symbol}()
  for (id, my_node) in enumerate(node)
    if id == 1 # node 1 is always [html ...]
      push!(creates, @transient query_0(String) => UInt64)
      push!(merges, @merge begin
        session(session)
        return query_0(session) => hash(session)
      end)
      push!(group_ids, :group_0)
      push!(creates, @transient group_0(String, Int64) => (UInt64, UInt64, FixedNodeKind, String))
      push!(merges, @merge begin
        query_0(session) => query_hash
        my_hash = hash(0, query_hash)
        return group_0(session, 1) => (UInt64(0), my_hash, Html, "html")
      end)
      
    elseif my_node isa QueryNode
      my_hash = :(hash($id, query_parent_hash))
      for var in my_node.vars
        my_hash = :(hash($var, $my_hash))
      end
      push!(creates, @eval @transient $(Symbol("query_", id))($(types[id])...) => UInt64)
      push!(merges, @eval @merge begin
        $(Symbol("query_", query_parent[id]))($(vars[query_parent[id]]...)) => query_parent_hash
        $(my_node.table)($(my_node.vars...))
        my_hash = $my_hash
        return $(Symbol("query_", id))($(vars[id]...)) => my_hash
      end)
      
    elseif my_node isa FixedNode 
      push!(group_ids, Symbol("group_", fixed_parent[id]))
      push!(creates, @eval @transient $(Symbol("group_", fixed_parent[id]))($(key_types[id]...)) => (UInt64, UInt64, FixedNodeKind, String))
      push!(merges, @eval @merge begin
        $(Symbol("group_", fixed_parent[fixed_parent[id]]))($(key_vars[fixed_parent[id]]...)) => (_, fixed_parent_hash, _, _)
        $(Symbol("query_", query_parent[id]))($(vars[query_parent[id]]...)) => query_parent_hash
        my_hash = hash($id, query_parent_hash)
        return $(Symbol("group_", fixed_parent[id]))($(key_exprs[id]...)) => (fixed_parent_hash, my_hash, $(my_node.kind), string($(my_node.content...)))
      end)
      
    elseif my_node isa AttributeNode
      push!(merges, @eval @merge begin
        $(Symbol("group_", fixed_parent[fixed_parent[id]]))($(key_vars[fixed_parent[id]]...)) => (_, fixed_parent_hash, _, _)
        $(Symbol("query_", query_parent[id]))($(vars[query_parent[id]]...)) => _
        return attribute(session, fixed_parent_hash, string($(my_node.key...))) => string($(my_node.val...))
      end)
    end
    
  end
  
  flow = Sequence(Flow[
    @transient attribute(String, UInt64, String) => String
    unique((flow) -> flow.output_name, creates)...
    merges...
  ])
  
  group_ids = unique(group_ids)
  @assert shift!(group_ids) == :group_0 # not a real group
  
  return Compiled(flow, group_ids)
end

# --- plumbing ---

mutable struct View
  world::World
  template::Any
  parsed::Parsed
  compiled::Compiled
  clients::Dict{String, WebSocket}
  server::Nullable{Server}
end

function View() 
  view = View(
    World(),
    quote [html] end, 
    Parsed(Node[], Int64[]), 
    Compiled(Sequence(Flow[]), Symbol[]),
    Dict{String, WebSocket}(),
    Nullable{Server}()
  )
  finalizer(view, close)
  view
end

function set_template!(view::View, template::ANY)
  view.template = template
  @showtime view.parsed = parse(template)
  @showtime view.compiled = compile(view.parsed.node, view.parsed.parent, (table, ix) -> eltype(view.world.state[table].columns[ix]))
  refresh(view)
end

function Flows.set_flow!(view::View, flow::Flow)
  view.world.flow = Sequence([
    @stateful session(String)
    flow
  ])
  set_template!(view, view.template) # need to recompile template in case the types have changed
end

function Flows.refresh(view::View)
  (old_state, _) = refresh(view.world)
  @showtime Flows.init_flow(view.compiled.flow, view.world)
  @showtime Flows.run_flow(view.compiled.flow, view.world)
  @showtime render(view, old_state, view.world.state)
  (old_state, view.world.state)
end

function Flows.refresh(view::View, event_table::Symbol, event_row::Tuple)
  (old_state, _) = refresh(view.world, event_table, event_row)
  @showtime Flows.init_flow(view.compiled.flow, view.world)
  @showtime Flows.run_flow(view.compiled.flow, view.world)
  @showtime render(view, old_state, view.world.state)
  (old_state, view.world.state)
end

macro js_str(text)
  parsed = Base.parse("\"$(escape_string(text))\"")
  if isa(parsed, Expr) && (parsed.head == :string)
    parsed = Expr(:string, [isa(arg, String) ? arg : :(json($arg)) for arg in parsed.args]...)
  end
  esc(parsed)
end

# TODO figure out how to handle changes in template
function render(view, old_state, new_state)
  old_groups = Dict{Symbol, Relation}(group_id => get(() -> empty(new_state[group_id]), old_state, group_id) for group_id in view.compiled.group_ids)
  old_attributes::Relation{Tuple{Vector{String}, Vector{UInt64}, Vector{String}, Vector{String}}} = get(() -> empty(new_state[:attribute]), old_state, :attribute)
  new_groups = Dict{Symbol, Relation}(group_id => new_state[group_id] for group_id in view.compiled.group_ids)
  new_attributes::Relation{Tuple{Vector{String}, Vector{UInt64}, Vector{String}, Vector{String}}} = new_state[:attribute]
  node_delete_parents = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  node_delete_childs = Set{UInt64}()
  node_delete_ixes = Dict(session => Vector{Int64}() for (session, _) in view.clients)
  html_create_parents = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  html_create_ixes = Dict(session => Vector{Int64}() for (session, _) in view.clients)
  html_create_childs = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  html_create_tags = Dict(session => Vector{String}() for (session, _) in view.clients)
  text_create_parents = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  text_create_ixes = Dict(session => Vector{Int64}() for (session, _) in view.clients)
  text_create_contents = Dict(session => Vector{String}() for (session, _) in view.clients)
  for group_id in view.compiled.group_ids
    (olds, news) = Data.diff_ixes(old_groups[group_id], new_groups[group_id])
    old_columns = old_groups[group_id].columns
    new_columns = new_groups[group_id].columns
    old_sessions::Vector{String} = old_columns[1]
    old_parent_ids::Vector{UInt64} = old_columns[end-3]
    old_child_ids::Vector{UInt64} = old_columns[end-2]
    new_sessions::Vector{String} = new_columns[1]
    new_parent_ids::Vector{UInt64} = new_columns[end-3]
    new_child_ids::Vector{UInt64} = new_columns[end-2]
    new_kinds::Vector{FixedNodeKind} = new_columns[end-1]
    new_contents::Vector{String} = new_columns[end-0]
    for i in olds
      push!(node_delete_childs, old_child_ids[i])
      if !(old_parent_ids[i] in node_delete_childs)
        session = old_sessions[i]
        parent = old_parent_ids[i]
        (next_i, _) = gallop(old_parent_ids, parent, i, length(old_parent_ids)+1, 1)
        ix = next_i - i
        push!(node_delete_parents[session], parent)
        push!(node_delete_ixes[session], ix)
      end
    end
    for i in reverse(news) # go backwards so that ixes are correct by the time they are reached
      session = new_sessions[i]
      parent = new_parent_ids[i]
      (next_i, _) = gallop(new_parent_ids, parent, i, length(new_parent_ids)+1, 1)
      ix = next_i - i - 1 
      if new_kinds[i] == Html
        push!(html_create_parents[session], parent)
        push!(html_create_ixes[session], ix)
        push!(html_create_childs[session], new_child_ids[i])
        push!(html_create_tags[session], new_contents[i])
      else
        push!(text_create_parents[session], parent)
        push!(text_create_ixes[session], ix)
        push!(text_create_contents[session], new_contents[i])
      end
    end
  end
  old_sessions = old_attributes.columns[1]
  old_childs::Vector{UInt64} = old_attributes.columns[2]
  old_keys::Vector{String} = old_attributes.columns[3]
  new_sessions = new_attributes.columns[1]
  new_childs::Vector{UInt64} = new_attributes.columns[2]
  new_keys::Vector{String} = new_attributes.columns[3]
  new_vals::Vector{String} = new_attributes.columns[4]
  (olds, news) = Data.diff_ixes(old_attributes, new_attributes)
  attribute_delete_childs = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  attribute_delete_keys = Dict(session => Vector{String}() for (session, _) in view.clients)
  for i in olds
    if !(old_childs[i] in node_delete_childs)
      session = old_sessions[i]
      push!(attribute_delete_childs[session], old_childs[i])
      push!(attribute_delete_keys[session], old_keys[i])
    end
  end
  attribute_create_childs = Dict(session => Vector{UInt64}() for (session, _) in view.clients)
  attribute_create_keys = Dict(session => Vector{String}() for (session, _) in view.clients)
  attribute_create_vals = Dict(session => Vector{String}() for (session, _) in view.clients)
  for i in news
    session = new_sessions[i]
    push!(attribute_create_childs[session], new_childs[i])
    push!(attribute_create_keys[session], new_keys[i])
    push!(attribute_create_vals[session], new_vals[i])
  end
  for (session, client) in view.clients
    # TODO the implicit unchecked UInt64 -> JSFloat is probably going to be trouble sooner or later
    write(client, js"{\"events\": $(view.world.events), \"render\": [$(node_delete_parents[session]), $(node_delete_ixes[session]), $(html_create_parents[session]), $(html_create_ixes[session]), $(html_create_childs[session]), $(html_create_tags[session]), $(text_create_parents[session]), $(text_create_ixes[session]), $(text_create_contents[session]), $(attribute_delete_childs[session]), $(attribute_delete_keys[session]), $(attribute_create_childs[session]), $(attribute_create_keys[session]), $(attribute_create_vals[session])]}")
  end
end

function serve(view)
  handler = WebSocketHandler() do req,client
    begin
      session = string(now()) # TODO uuid
      write(client, js"{\"session\": $session}")
      view.clients[session] = client
      refresh(view, :session, tuple(session))
      while true
        bytes = read(client)
        @showtime event = JSON.parse(String(bytes))
        @showtime refresh(view, Symbol(event["table"]), tuple(event["values"]...))
      end
    end
  end
  server = Server(handler)
  @async run(server,8080)
  view.server = Nullable(server)
  server
end

function Base.close(view::View)
  if !isnull(view.server) && isopen(get(view.server).http.sock)
    close(get(view.server))
  end
  for (_, client) in view.clients
    if isopen(client)
      close(client)
    end
  end
  view.server = Nullable{Server}()
  view.clients = Dict{String, WebSocket}()
end

export View, set_template!, serve

end
