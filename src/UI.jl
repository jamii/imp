module UI

using Data
using Query
using Flows
using Match
using HttpServer
using WebSockets
using JSON

# --- ast ---

immutable StringExpr
  values::Vector{Union{String, Symbol}}
end

typealias Value Union{StringExpr, String, Symbol} 

abstract Node

immutable AttributeNode <: Node
  key::Value
  val::Value
end

immutable FixedNode <: Node
  tag::Value
  kind::Symbol # :text or :html
  children::Vector{Node}
end

immutable QueryNode <: Node
  table::Symbol
  vars::Vector{Symbol}
  children::Vector{Node}
end

# --- parsing ---

function preprocess(expr)
  @match expr begin
    Expr(:vect || :vcat || :hcat, args, _) => map(preprocess, args)
    Expr(head, args, _) => Expr(head, map(preprocess, args)...)
    args::Vector => args
    value::Union{Symbol, String, Number} => value
    _ => error("What are this? $expr")
  end
end

function parse_value(expr)
  @match expr begin
    _::String => expr
    _::Symbol => expr
    Expr(:string, args, _) => StringExpr(map(parse_value, args))
    _ => error("What are this? $expr")
  end
end

function is_line_number(expr)
  @match expr begin
    Expr(:line, _, _) => true
    _ => false
  end
end

function parse_nodes(exprs)
  map(parse_node, exprs)
end

function parse_node(expr) 
  @match expr begin
    Expr(:call, [table::Symbol, children_expr, vars...], _) => begin
      @match children_expr begin
        Expr(:->, [Expr(:tuple, [], _), Expr(:block, children_exprs, _)], _) => begin
          children = parse_nodes(filter((e) -> !is_line_number(e), children_exprs))
          QueryNode(table, vars, children)
        end
        _ => error("What are this? $children_expr")
      end
    end
    [tag, args...] => FixedNode(parse_value(tag), :html, parse_nodes(args))
    Expr(:(=), [key, val], _) => AttributeNode(parse_value(key), parse_value(val))
    text => FixedNode(parse_value(text), :text, Node[])
  end
end

function parse_template(expr)
  @match expr begin
    Expr(:block, [Expr(:line, _, _), child], _) => parse_node(preprocess(child))
    _ => parse_node(preprocess(expr))
  end
end

# --- compiling ---

function value_expr(value::Value) 
  @match value begin
    _::String => value
    _::Symbol => string(value)
    _::StringExpr => Expr(:string, value.values...)
  end
end

function compile_server_tree(node::AttributeNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, state, flows)
  merge_flow = @eval @merge begin
    $fixed_parent_id($(map(first, fixed_parent_vars)...)) => parent_id
    $parent_id($(map(first, parent_vars)...)) => _
    return attribute(parent_id, $(value_expr(node.key))) => $(value_expr(node.val))
  end
  push!(flows, merge_flow)
end

function compile_server_tree(node::FixedNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, state, flows)
  id = Symbol("node_$(hash(node))")
  transient_flow = @eval @transient $id($([typ for (_, typ) in parent_vars]...)) => UInt64
  merge_flow = 
    if parent_id == nothing
      @eval @merge begin
        session(session)
        child_id = hash(session, $(hash(node)))
        return $id($(map(first, parent_vars)...)) => child_id
      end
    else
      @eval @merge begin
        $parent_id($(map(first, parent_vars)...)) => parent_id
        child_id = hash(parent_id, $(hash(node)))
        return $id($(map(first, parent_vars)...)) => child_id
      end
    end
  push!(flows, transient_flow, merge_flow)
  for child in node.children
    compile_server_tree(child, id, parent_vars, id, parent_vars, state, flows)
  end
end

function compile_server_tree(node::QueryNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, state, flows)
  id = Symbol("node_$(hash(node))")
  vars = copy(parent_vars)
  start_ix = length(vars) + 1
  for (ix, var) in enumerate(node.vars)
    if (var != :(_)) 
      typ = eltype(state[node.table].columns[ix])
      typed_var = (var, typ)
      if !(typed_var in parent_vars)
        push!(vars, (var, typ))
      end
    end
  end
  end_ix = length(vars)
  transient_flow = @eval @transient $id($([typ for (_, typ) in vars]...)) => UInt64
  child_id = :(hash(parent_id, $(hash(node))))
  for (var, _) in vars[start_ix:end_ix]
    child_id = :(hash($var, $child_id))
  end
  merge_flow = @eval @merge begin
    $parent_id($(map(first, parent_vars)...)) => parent_id
    $(node.table)($(node.vars...))
    child_id = $child_id
    return $id($(map(first, vars)...)) => child_id
  end
  push!(flows, transient_flow, merge_flow)
  for child in node.children
    compile_server_tree(child, id, vars, fixed_parent_id, fixed_parent_vars, state, flows)
  end
end

function collect_sort_key(node::AttributeNode, parent_vars, state, key, keyed_children)
  # nothing to do here
end

function collect_sort_key(node::FixedNode, parent_vars, state, key, keyed_children)
  push!(keyed_children, (copy(key), copy(parent_vars), node))
end

function collect_sort_key(node::QueryNode, parent_vars, state, key, keyed_children)
  vars = copy(parent_vars)
  start_ix = length(key) + 1
  for (ix, var) in enumerate(node.vars)
    if (var != :(_)) 
      typ = eltype(state[node.table].columns[ix])
      typed_var = (var, typ)
      if !(typed_var in parent_vars)
        push!(vars, (var, typ))
        push!(key, (var, typ))
      end
    end
  end
  end_ix = length(key)
  collect_sort_key(node.children, vars, state, key, keyed_children)
  for ix in start_ix:end_ix
    key[ix] = (nothing, key[ix][2])
  end
end

function collect_sort_key(nodes::Vector{Node}, parent_vars, state, key, keyed_children)
  push!(key, 0)
  ix = length(key)
  for node in nodes
    if typeof(node) in [FixedNode, QueryNode] 
      key[ix] += 1
      collect_sort_key(node, parent_vars, state, key, keyed_children)
    end
  end
  key[ix] = 0
end

function collect_sort_key(node::FixedNode, parent_vars, state)
  key = copy(parent_vars)
  keyed_children = Any[]
  collect_sort_key(node.children, parent_vars, state, key, keyed_children)
  # tidy up ragged ends of keys
  for (child_key, vars, child) in keyed_children
    append!(child_key, key[(length(child_key)+1):end])
  end
  keyed_children
end

# TODO this is defined in Julia 0.6 but can't currently upgrade because of https://github.com/kmsquire/Match.jl/issues/35
function Base.isless{T}(x::Nullable{T}, y::Nullable{T})
  !Base.isnull(x) && (Base.isnull(y) || (get(x) < get(y)))
end

default{T <: Number}(::Type{T}) = zero(T)
default(::Type{String}) = ""

function key_expr(elem) 
  @match elem begin
    _::Integer => elem
    (_::Symbol, _::Type) => elem[1]
    (_::Void, _::Type) => default(elem[2])
    _ => error("What are this: $elem")
  end
end

function key_type(elem)
  @match elem begin
    _::Integer => Int64
    (var::Symbol, typ::Type) => typ
    (_::Void, typ::Type) => typ
    _ => error("What are this: $elem")
  end
end

function compile_client_tree(node::FixedNode, parent_vars, state, flows, group_ids)
  keyed_children = collect_sort_key(node, parent_vars, state)
  group_id = Symbol("group_$(hash(node))")
  parent_node_id = Symbol("node_$(hash(node))")
  if !isempty(keyed_children)
    push!(group_ids, group_id)
  end
  for (key, child_vars, child) in keyed_children
    child_node_id = Symbol("node_$(hash(child))")
    key_exprs = map(key_expr, key)
    key_types = map(key_type, key)
    transient_flow = @eval @transient $group_id($(key_types...)) => (UInt64, UInt64, Symbol, String)
    merge_flow = @eval @merge begin
      $parent_node_id($(map(first, parent_vars)...)) => parent_id
      $child_node_id($(map(first, child_vars)...)) => child_id
      return $group_id($(key_exprs...)) => (parent_id, child_id, $(Expr(:quote, child.kind)), $(value_expr(child.tag)))
    end
    push!(flows, transient_flow, merge_flow)
  end
  for (_, child_vars, child) in keyed_children
    compile_client_tree(child, child_vars, state, flows, group_ids)
  end
end

function compile_template(node, state)
  flows = Flow[@transient attribute(id::UInt64, key::String) => value::String]
  group_ids = Symbol[]
  compile_server_tree(node, nothing, [(:session, String)], nothing, [:session], state, flows)
  compile_client_tree(node, Any[(:session, String)], state, flows, group_ids)
  (Sequence(flows), group_ids)
end

# --- plumbing ---

type View
  world::World
  template::Any
  parsed::Node
  compiled::Flow
  group_names::Vector{Symbol}
  clients::Dict{String, WebSocket}
  server::Nullable{Server}
end

function Base.close(view::View)
  if !isnull(view.server)
    close(get(view.server))
  end
  for (_, client) in view.clients
    close(client)
  end
end

function View() 
  view = View(
    World(),
    quote [div] end, 
    FixedNode("body", :html, [FixedNode("loading...", :text, Node[])]), 
    Sequence(Flow[]), 
    Symbol[],
    Dict{String, WebSocket}(),
    Nullable{Server}()
  )
  finalizer(view, close)
  view
end

function set_template!(view::View, template::ANY)
  view.template = template
  view.parsed = parse_template(template)
  (view.compiled, view.group_names) = compile_template(view.parsed, view.world.state)
  refresh(view)
end

function Flows.set_flow!(view::View, flow::Flow)
  set_flow!(view.world, flow)
  set_template!(view, view.template) # need to recompile template in case the types have changed
  refresh(view)
end

pre = Sequence([
  @stateful session(String)
])

function Flows.refresh(view::View)
  Flows.init_flow(pre, view.world)
  (old_state, _) = refresh(view.world)
  @show @time Flows.init_flow(view.compiled, view.world)
  @show @time Flows.run_flow(view.compiled, view.world)
  @show @time render(view, old_state, view.world.state)
  (old_state, view.world.state)
end

function Flows.refresh(view::View, event_table::Symbol, event_row::Tuple)
  Flows.init_flow(pre, view.world)
  (old_state, _) = refresh(view.world, event_table, event_row)
  @show @time Flows.init_flow(view.compiled, view.world)
  @show @time Flows.run_flow(view.compiled, view.world)
  @show @time render(view, old_state, view.world.state)
  (old_state, view.world.state)
end

macro js_str(text)
  parsed = parse("\"$(escape_string(text))\"")
  if isa(parsed, Expr) && (parsed.head == :string)
    parsed = Expr(:string, [isa(arg, String) ? arg : :(json($arg)) for arg in parsed.args]...)
  end
  esc(parsed)
end

# TODO figure out how to handle changes in template
function render(view, old_state, new_state)
  for (_, client) in view.clients
    write(client, js"{\"events\": $(view.world.events)}")
  end
  old_groups = Dict{Symbol, Relation}(name => get(() -> empty(new_state[name]), old_state, name) for name in view.group_names)
  old_attributes::Relation{Tuple{Vector{UInt64}, Vector{String}, Vector{String}}} = get(() -> empty(new_state[:attribute]), old_state, :attribute)
  new_groups = Dict{Symbol, Relation}(name => new_state[name] for name in view.group_names)
  new_attributes::Relation{Tuple{Vector{UInt64}, Vector{String}, Vector{String}}} = new_state[:attribute]
  node_delete_parents = Vector{UInt64}()
  node_delete_childs = Set{UInt64}()
  node_delete_ixes = Vector{Int64}()
  html_create_parents = Vector{UInt64}()
  html_create_ixes = Vector{Int64}()
  html_create_childs = Vector{UInt64}()
  html_create_tags = Vector{String}()
  text_create_parents = Vector{UInt64}()
  text_create_ixes = Vector{Int64}()
  text_create_contents = Vector{String}()
  for name in view.group_names
    (olds, news) = Data.diff_ixes(old_groups[name], new_groups[name])
    old_columns = old_groups[name].columns
    new_columns = new_groups[name].columns
    old_parent_ids::Vector{UInt64} = old_columns[end-3]
    old_child_ids::Vector{UInt64} = old_columns[end-2]
    new_parent_ids::Vector{UInt64} = new_columns[end-3]
    new_child_ids::Vector{UInt64} = new_columns[end-2]
    new_kinds::Vector{Symbol} = new_columns[end-1]
    new_contents::Vector{String} = new_columns[end-0]
    for i in olds
      push!(node_delete_childs, old_child_ids[i])
      if !(old_parent_ids[i] in node_delete_childs)
        parent = old_parent_ids[i]
        (next_i, _) = gallop(old_parent_ids, parent, i, length(old_parent_ids)+1, 1)
        ix = next_i - i
        push!(node_delete_parents, parent)
        push!(node_delete_ixes, ix)
      end
    end
    for i in reverse(news) # go backwards so that ixes are correct by the time they are reached
      parent = new_parent_ids[i]
      (next_i, _) = gallop(new_parent_ids, parent, i, length(new_parent_ids)+1, 1)
      ix = next_i - i - 1 
      if new_kinds[i] == :html
        push!(html_create_parents, parent)
        push!(html_create_ixes, ix)
        push!(html_create_childs, new_child_ids[i])
        push!(html_create_tags, new_contents[i])
      else
        push!(text_create_parents, parent)
        push!(text_create_ixes, ix)
        push!(text_create_contents, new_contents[i])
      end
    end
  end
  (olds, news) = Data.diff_ixes(old_attributes, new_attributes)
  attribute_delete_childs = old_attributes.columns[1][olds]
  attribute_delete_keys = old_attributes.columns[2][olds]
  attribute_create_childs = new_attributes.columns[1][news]
  attribute_create_keys = new_attributes.columns[2][news]
  attribute_create_vals = new_attributes.columns[3][news]
  nonredundant_attribute_delete_childs = empty(attribute_delete_childs)
  nonredundant_attribute_delete_keys = empty(attribute_delete_keys)
  for (i, child) in enumerate(attribute_delete_childs)
    if !(child in node_delete_childs)
      push!(nonredundant_attribute_delete_childs, child)
      push!(nonredundant_attribute_delete_keys, attribute_delete_keys[i])
    end
  end
  for (_, client) in view.clients
    # TODO handle sessions
    # TODO the implicit unchecked UInt64 -> JSFloat is probably going to be trouble sooner or later
    write(client, js"{\"render\": [$node_delete_parents, $node_delete_ixes, $html_create_parents, $html_create_ixes, $html_create_childs, $html_create_tags, $text_create_parents, $text_create_ixes, $text_create_contents, $nonredundant_attribute_delete_childs, $nonredundant_attribute_delete_keys, $attribute_create_childs, $attribute_create_keys, $attribute_create_vals]}")
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
        event = JSON.parse(String(read(client)))
        refresh(view, Symbol(event["table"]), tuple(event["values"]...))
      end
    end
  end
  server = Server(handler)
  @async run(server,8080)
  view.server = Nullable(server)
end

export View, set_template!, serve

end
