module UI

using Util
using Data
using Query
using Flows
using Match
using HttpServer
using WebSockets
using JSON

const Session = Int64 

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
  attribute_ids::Vector{Symbol}
end

function string_expr(values)
  if all(value isa String for value in values)
    string(values...)
  else
    :(string($(values...)))
  end
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
  types = Dict{Int64, Vector{Type}}(0 => [Session])
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
  key = Dict{Int64, Vector{KeyElem}}(0 => KeyElem[(:session, Session)])
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
  attribute_ids = Vector{Symbol}()
  for (id, my_node) in enumerate(node)
    if id == 1 # node 1 is always [html ...]
      push!(creates, @transient query_0(Session) => UInt64)
      push!(merges, @merge begin
        session(session)
        return query_0(session) => hash(session)
      end)
      push!(group_ids, :group_0)
      push!(creates, @transient group_0(Session, Int64) => (UInt64, UInt64, FixedNodeKind, String))
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
        return $(Symbol("group_", fixed_parent[id]))($(key_exprs[id]...)) => (fixed_parent_hash, my_hash, $(my_node.kind), $(string_expr(my_node.content)))
      end)
      
    elseif my_node isa AttributeNode
      push!(attribute_ids, Symbol("attribute_", id))
      push!(creates, @eval @transient $(Symbol("attribute_", id))(Session, UInt64, String) => String)
      push!(merges, @eval @merge begin
        $(Symbol("group_", fixed_parent[fixed_parent[id]]))($(key_vars[fixed_parent[id]]...)) => (_, fixed_parent_hash, _, _)
        $(Symbol("query_", query_parent[id]))($(vars[query_parent[id]]...)) => _
        return $(Symbol("attribute_", id))(session, fixed_parent_hash, $(string_expr(my_node.key))) => $(string_expr(my_node.val))
      end)
    end
    
  end
  
  flow = Sequence(Flow[
    unique((flow) -> flow.output_name, creates)...
    merges...
  ])
  
  group_ids = unique(group_ids)
  @assert shift!(group_ids) == :group_0 # not a real group
  
  attribute_ids = unique(attribute_ids)
  
  return Compiled(flow, group_ids, attribute_ids)
end

# --- plumbing ---

mutable struct View
  world::World
  template::Any
  parsed::Parsed
  compiled::Compiled
  clients::Dict{Session, WebSocket}
  server::Nullable{Server}
end

function View() 
  view = View(
    World(),
    quote [html] end, 
    Parsed(Node[], Int64[]), 
    Compiled(Sequence(Flow[]), Symbol[], Symbol[]),
    Dict{Session, WebSocket}(),
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
    @stateful session(Session)
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

# TODO use subarrays instead of verbosely creating all these dicts
function render(view, old_state, new_state)
  sessions = Set{Session}(vcat(get(() -> new_state[:query_0], old_state, :query_0).columns[1], new_state[:query_0].columns[1]))

  deleted_nodes = Set{UInt64}()
  node_delete_childs = Dict(session => Vector{UInt64}() for session in sessions)
  html_create_parents = Dict(session => Vector{UInt64}() for session in sessions)
  html_create_siblings = Dict(session => Vector{UInt64}() for session in sessions)
  html_create_childs = Dict(session => Vector{UInt64}() for session in sessions)
  html_create_tags = Dict(session => Vector{String}() for session in sessions)
  text_create_parents = Dict(session => Vector{UInt64}() for session in sessions)
  text_create_siblings = Dict(session => Vector{UInt64}() for session in sessions)
  text_create_childs = Dict(session => Vector{UInt64}() for session in sessions)
  text_create_contents = Dict(session => Vector{String}() for session in sessions)
  for group_id in view.compiled.group_ids
    new_group = new_state[group_id]
    old_group = get(() -> empty(new_group), old_state, group_id)
    (deleted, created) = Data.diff_ixes(old_group, new_group)
  
    old_sessions::Vector{Session} = old_group.columns[1]
    old_parent_ids::Vector{UInt64} = old_group.columns[end-3]
    old_child_ids::Vector{UInt64} = old_group.columns[end-2]
    for i in deleted
      push!(deleted_nodes, old_child_ids[i])
      if !(old_parent_ids[i] in deleted_nodes)
        session = old_sessions[i]
        push!(node_delete_childs[session], old_child_ids[i])
      end
    end
  
    new_sessions::Vector{Session} = new_group.columns[1]
    new_parent_ids::Vector{UInt64} = new_group.columns[end-3]
    new_child_ids::Vector{UInt64} = new_group.columns[end-2]
    new_kinds::Vector{FixedNodeKind} = new_group.columns[end-1]
    new_contents::Vector{String} = new_group.columns[end-0]
    for i in reverse(created) # reversed, so that sibling exists by the time child is added
      session = new_sessions[i]
      parent = new_parent_ids[i]
      if new_kinds[i] == Html
        push!(html_create_parents[session], new_parent_ids[i])
        push!(html_create_childs[session], new_child_ids[i])
        push!(html_create_tags[session], new_contents[i])
        if (i == length(new_parent_ids)) || (new_sessions[i+1] != new_sessions[i]) || (new_parent_ids[i+1] != new_parent_ids[i])
          push!(html_create_siblings[session], UInt64(0))
        else
          push!(html_create_siblings[session], new_child_ids[i+1])
        end
      else
        push!(text_create_parents[session], parent)
        push!(text_create_childs[session], new_child_ids[i])
        push!(text_create_contents[session], new_contents[i])
        if (i == length(new_parent_ids)) || (new_sessions[i+1] != new_sessions[i]) || (new_parent_ids[i+1] != new_parent_ids[i])
          push!(text_create_siblings[session], UInt64(0))
        else
          push!(text_create_siblings[session], new_child_ids[i+1])
        end
      end
    end
  end
  
  attribute_delete_childs = Dict(session => Vector{UInt64}() for session in sessions)
  attribute_delete_keys = Dict(session => Vector{String}() for session in sessions)
  attribute_create_childs = Dict(session => Vector{UInt64}() for session in sessions)
  attribute_create_keys = Dict(session => Vector{String}() for session in sessions)
  attribute_create_vals = Dict(session => Vector{String}() for session in sessions)
  for attribute_id in view.compiled.attribute_ids
    new_attributes::Relation{Tuple{Vector{Session}, Vector{UInt64}, Vector{String}, Vector{String}}} = new_state[attribute_id]
    old_attributes::Relation{Tuple{Vector{Session}, Vector{UInt64}, Vector{String}, Vector{String}}} = get(old_state, attribute_id, new_attributes)
    (deleted, created) = Data.diff_ixes(old_attributes, new_attributes)
    
    old_sessions = old_attributes.columns[1]
    old_childs::Vector{UInt64} = old_attributes.columns[2]
    old_keys::Vector{String} = old_attributes.columns[3]
    for i in deleted
      if !(old_childs[i] in deleted_nodes)
        session = old_sessions[i]
        push!(attribute_delete_childs[session], old_childs[i])
        push!(attribute_delete_keys[session], old_keys[i])
      end
    end
    
    new_sessions = new_attributes.columns[1]
    new_childs::Vector{UInt64} = new_attributes.columns[2]
    new_keys::Vector{String} = new_attributes.columns[3]
    new_vals::Vector{String} = new_attributes.columns[4]
    for i in created
      session = new_sessions[i]
      push!(attribute_create_childs[session], new_childs[i])
      push!(attribute_create_keys[session], new_keys[i])
      push!(attribute_create_vals[session], new_vals[i])
    end
  end
  
  for (session, client) in view.clients
    # TODO the implicit unchecked UInt64 -> JSFloat is probably going to be trouble sooner or later
    try
      write(client, js"{\"events\": $(view.world.events), \"render\": [$(node_delete_childs[session]), $(html_create_parents[session]), $(html_create_siblings[session]), $(html_create_childs[session]), $(html_create_tags[session]), $(text_create_parents[session]), $(text_create_siblings[session]), $(text_create_childs[session]), $(text_create_contents[session]), $(attribute_delete_childs[session]), $(attribute_delete_keys[session]), $(attribute_create_childs[session]), $(attribute_create_keys[session]), $(attribute_create_vals[session])]}")
    catch error
      info(error)
      delete!(view.clients, session)
      view.world.state[:session] = Relation((filter((s) -> s != session, view.world.state[:session].columns[1]),), 1)
    end
  end
end

function serve(view)
  handler = WebSocketHandler() do req,client
    begin
      session = Dates.value(now()) # TODO uuid
      write(client, js"{\"session\": $session}")
      view.clients[session] = client
      refresh(view, :session, tuple(session))
      while true
        bytes = try
          read(client)
        catch error
          info(error)
          delete!(view.clients, session)
          view.world.state[:session] = Relation((filter((s) -> s != session, view.world.state[:session].columns[1]),), 1)
          return
        end
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
    try
      close(get(view.server))
    catch error
      warn(error)
    end
  end
  for (_, client) in view.clients
    if isopen(client)
      try
        close(client)
      catch error
        warn(error)
      end
    end
  end
  view.server = Nullable{Server}()
  empty!(view.clients)
end

export Session, View, set_template!, serve, js_str

end
