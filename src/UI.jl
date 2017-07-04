module UI

# Pkg.add("Blink")
# AtomShell.install()

using Data
using Query
using Flows
using Blink
using Match
using Hiccup

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

function flatten_value(value::Value) 
  @match value begin
    _::String => [value]
    _::Symbol => [string(value)]
    _::StringExpr => value.values
  end
end

function compile_server_tree(node::AttributeNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, flows)
  merge_flow = @eval @merge begin
    $fixed_parent_id($(fixed_parent_vars...)) => parent_id
    $parent_id($(parent_vars...)) => _
    return attribute(parent_id, string($(flatten_value(node.key)...))) => string($(flatten_value(node.val)...))
  end
  push!(flows, merge_flow)
end

function compile_server_tree(node::FixedNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, flows)
  id = Symbol("node_$(hash(node))")
  transient_flow = @eval @transient $id($([Any for _ in parent_vars]...)) => UInt64
  merge_flow = 
    if parent_id == nothing
      @eval @merge begin
        session(session)
        child_id = hash(session, $(hash(node)))
        return $id($(parent_vars...)) => child_id
      end
    else
      @eval @merge begin
        $parent_id($(parent_vars...)) => parent_id
        child_id = hash(parent_id, $(hash(node)))
        return $id($(parent_vars...)) => child_id
      end
    end
  push!(flows, transient_flow, merge_flow)
  for child in node.children
    compile_server_tree(child, id, parent_vars, id, parent_vars, flows)
  end
end

function compile_server_tree(node::QueryNode, parent_id, parent_vars, fixed_parent_id, fixed_parent_vars, flows)
  id = Symbol("node_$(hash(node))")
  node_real_vars = filter((var) -> var != :(_), node.vars)
  vars = unique(vcat(parent_vars, node_real_vars))
  transient_flow = @eval @transient $id($([Any for _ in vars]...)) => UInt64
  merge_flow = @eval @merge begin
    $parent_id($(parent_vars...)) => parent_id
    $(node.table)($(node.vars...))
    child_id = hash(tuple($(node_real_vars...), parent_id), $(hash(node)))
    return $id($(vars...)) => child_id
  end
  push!(flows, transient_flow, merge_flow)
  for child in node.children
    compile_server_tree(child, id, vars, fixed_parent_id, fixed_parent_vars, flows)
  end
end

function collect_sort_key(node::AttributeNode, parent_vars, key, keyed_children)
  # nothing to do here
end

function collect_sort_key(node::FixedNode, parent_vars, key, keyed_children)
  push!(keyed_children, (copy(key), parent_vars, node))
end

function collect_sort_key(node::QueryNode, parent_vars, key, keyed_children)
  node_real_vars = filter((var) -> var != :(_), node.vars)
  vars = unique(vcat(parent_vars, node_real_vars))
  new_vars = vars[(1+length(parent_vars)):end]
  start_ix = length(key) + 1
  append!(key, new_vars) 
  end_ix = length(key)
  collect_sort_key(node.children, vars, key, keyed_children)
  for ix in start_ix:end_ix
    key[ix] = nothing
  end
end

function collect_sort_key(nodes::Vector{Node}, parent_vars, key, keyed_children)
  push!(key, 0)
  ix = length(key)
  for node in nodes
    if typeof(node) in [FixedNode, QueryNode] # TODO handle attributes and text
      key[ix] += 1
      collect_sort_key(node, parent_vars, key, keyed_children)
    end
  end
  key[ix] = 0
end

function collect_sort_key(node::FixedNode, parent_vars)
  key = Vector{Any}(parent_vars)
  keyed_children = Any[]
  collect_sort_key(node.children, parent_vars, key, keyed_children)
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

function key_expr(elem) 
  @match elem begin
    _::Integer => elem
    _::Symbol => :(Nullable($elem))
    _::Void => :(Nullable())
    _ => error("What are this: $elem")
  end
end

function key_type(elem)
  @match elem begin
    _::Integer => Int64
    _::Symbol => Nullable{Any} # TODO figure out correct types somehow
    _::Void => Nullable{Any}
    _ => error("What are this: $elem")
  end
end

function compile_client_tree(node::FixedNode, parent_vars, flows, group_ids)
  keyed_children = collect_sort_key(node, parent_vars)
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
      $parent_node_id($(parent_vars...)) => parent_id
      $child_node_id($(child_vars...)) => child_id
      return $group_id($(key_exprs...)) => (parent_id, child_id, $(Expr(:quote, child.kind)), string($(flatten_value(child.tag)...)))
    end
    push!(flows, transient_flow, merge_flow)
  end
  for (_, child_vars, child) in keyed_children
    compile_client_tree(child, child_vars, flows, group_ids)
  end
end

function compile_template(node)
  flows = Flow[@transient attribute(id::UInt64, key::String) => value::String]
  group_ids = Symbol[]
  compile_server_tree(node, nothing, [:session], nothing, [:session], flows)
  compile_client_tree(node, [:session], flows, group_ids)
  (Sequence(flows), group_ids)
end

# --- plumbing ---

type View
  template::Any
  parsed::Node
  compiled::Flow
  group_names::Vector{Symbol}
  watchers::Set{Any}
end

function View() 
  View(
    nothing, 
    FixedNode("body", :html, [FixedNode("loading...", :text, Node[])]), 
    Sequence(Flow[]), 
    Symbol[], 
    Set{Any}()
  )
end

function set_template!(view::View, template)
  view.template = template
  view.parsed = parse_template(template)
  (view.compiled, view.group_names) = compile_template(view.parsed)
  for watcher in view.watchers
    watcher()
  end
end

function Flows.watch(watcher, view::View)
  push!(view.watchers, watcher)
end

# TODO figure out how to handle changes in template
# TODO handle sessions
function render(window, view, world, session)
  for event in world.events
    js(window, Blink.JSString("$event = imp_event(\"$event\")"), callback=false) # these never return! :(
  end
  old_groups = Dict{Symbol, Union{Relation, Void}}(name => get(world.state, name, nothing) for name in view.group_names)
  old_attributes = get(world.state, :attribute, nothing)
  Flows.init_flow(view.compiled, world)
  Flows.run_flow(view.compiled, world)
  new_groups = Dict{Symbol, Relation}(name => world.state[name] for name in view.group_names)
  new_attributes = world.state[:attribute]
  for name in view.group_names
    if old_groups[name] == nothing
      old_groups[name] = empty(new_groups[name])
    end
  end
  if old_attributes == nothing
    old_attributes = empty(new_attributes)
  end
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
    old_columns = old_groups[name].columns
    old_parent_ids = old_columns[end-3]
    old_child_ids = old_columns[end-2]
    new_columns = new_groups[name].columns
    new_parent_ids = new_columns[end-3]
    new_child_ids = new_columns[end-2]
    new_kinds = new_columns[end-1]
    new_contents = new_columns[end-0]
    Data.foreach_diff(old_columns, new_columns, old_columns[1:end-4], new_columns[1:end-4],
      (o, i) -> begin
        push!(node_delete_childs, old_child_ids[i])
        if !(old_parent_ids[i] in node_delete_childs)
          parent = old_parent_ids[i]
          prev_i = findprev((prev_parent) -> prev_parent != parent, old_parent_ids, i-1)
          ix = i - prev_i - 1 # 0-indexed
          push!(node_delete_parents, parent)
          push!(node_delete_ixes, ix)
        end
      end,
      (n, i) -> begin
        parent = new_parent_ids[i]
        prev_i = findprev((prev_parent) -> prev_parent != parent, new_parent_ids, i-1)
        ix = i - prev_i - 1 # 0-indexed
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
      end,
      (o, n, oi, ni) -> ()
    )
  end
  # deletions have to be handled in reverse order to make sure the ixes are correct
  reverse!(node_delete_parents)
  reverse!(node_delete_ixes)
  ((attribute_delete_childs, attribute_delete_keys, _), (attribute_create_childs, attribute_create_keys, attribute_create_vals)) = Data.diff(old_attributes, new_attributes)
  nonredundant_attribute_deletes = [i for i in 1:length(attribute_delete_childs) if !(attribute_delete_childs[i] in node_delete_childs)]
  attribute_delete_childs = attribute_delete_childs[nonredundant_attribute_deletes]
  attribute_delete_keys = attribute_delete_keys[nonredundant_attribute_deletes]
  # TODO the implicit unchecked UInt64 -> JSFloat is probably going to be trouble sooner or later
  @js_(window, render($node_delete_parents, $node_delete_ixes, $html_create_parents, $html_create_ixes, $html_create_childs, $html_create_tags, $text_create_parents, $text_create_ixes, $text_create_contents, $attribute_delete_childs, $attribute_delete_keys, $attribute_create_childs, $attribute_create_keys, $attribute_create_vals))
end

function watch_and_load(window, file)
  load!(window, file)
  @schedule begin
    (waits, _) = open(`inotifywait -me CLOSE_WRITE $file`)
    while true
      readline(waits)
      load!(window, file)
    end
  end
end

pre = Sequence([
  @stateful session(String)
])

function window(world, view)
  window = Window()
  session = string(now()) # TODO uuid
  opentools(window)
  load!(window, "src/Imp.js")
  @js_(window, session = $session)
  sleep(3) # :(
  handle(window, "event") do event
    refresh(world, Symbol(event["table"]), tuple(event["values"]...))
  end
  watch(world) do old_state, new_state
    @time render(window, view, world, session)
  end
  watch(view) do
    @time render(window, view, world, session)
  end
  refresh(world, :session, tuple(session))
  window
end

export View, set_template!, window, watch

end
