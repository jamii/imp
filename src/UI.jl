module UI

# Pkg.add("Blink")
# AtomShell.install()

using Data
# using Query
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

immutable TextNode <: Node
  text::Value
end

immutable AttributeNode <: Node
  key::Value
  val::Value
end

immutable FixedNode <: Node
  tag::Value
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
    [tag, args...] => FixedNode(parse_value(tag), parse_nodes(args))
    Expr(:(=), [key, val], _) => AttributeNode(parse_value(key), parse_value(val))
    text => TextNode(parse_value(text))
  end
end

function parse_template(expr)
  @match expr begin
    Expr(:block, [Expr(:line, _, _), child], _) => parse_node(preprocess(child))
    _ => parse_node(preprocess(expr))
  end
end

# --- compiling ---

type CompiledQueryNode
  id::Symbol
  parent_id::Symbol
  bound_vars::Vector{Symbol}
  query_node::QueryNode
  fragment::Vector{Union{Symbol, String}}
  compiled_fragment::Function
end

function generate_fragment(value::Union{String, Symbol}, fragment)
  push!(fragment, string(value))
end

function generate_fragment(expr::StringExpr, fragment)
  for value in expr.values
    push!(fragment, value)
  end
end

function generate_fragment(node::TextNode, fragment)
  generate_fragment(node.text, fragment)
end

function generate_fragment(node::AttributeNode, fragment)
  push!(fragment, " ")
  generate_fragment(node.key, fragment)
  push!(fragment, "=")
  generate_fragment(node.val, fragment)
end

function generate_fragment(node::FixedNode, fragment)
  push!(fragment, "<")
  generate_fragment(node.tag, fragment)
  for child in node.children
    if typeof(child) == AttributeNode
      generate_fragment(child, fragment)
    end
  end
  push!(fragment, ">")
  for child in node.children
    if typeof(child) != AttributeNode
      generate_fragment(child, fragment)
    end
  end
  push!(fragment, "</")
  generate_fragment(node.tag, fragment)
  push!(fragment, ">")
end

function generate_fragment(node::QueryNode, fragment)
  # TODO no html generated, but do we need to record the position or something? maybe put a dummy node in?
end

function concat_fragment(fragment)
  new_fragment = Union{Symbol, String}[]
  for value in fragment 
    if isa(value, String) && (length(new_fragment) > 0) && isa(new_fragment[end], String)
      new_fragment[end] = string(new_fragment[end], value)
    else
      push!(new_fragment, value)
    end
  end
  new_fragment
end

function compile_fragment(id::Symbol, node::QueryNode, bound_vars::Vector{Symbol})
  fragment = Union{Symbol, String}[]
  for child in node.children
    generate_fragment(child, fragment)
  end
  fragment = concat_fragment(fragment)
  name = Symbol("fragment_", id)
  fun = @eval function $(name)($(bound_vars...))
    string($(fragment...))
  end
  (fragment, fun)
end

function compile_query_nodes(parent_id, parent_bound_vars, node, compiled_query_nodes)
  if typeof(node) == QueryNode
    id = Symbol("query_node_", hash((parent_id, node)))
    bound_vars = copy(parent_bound_vars)
    for var in node.vars
      if !(var in parent_bound_vars)
        push!(bound_vars, var)
      end
    end
    (fragment, compiled_fragment) = compile_fragment(id, node, bound_vars)
    compiled_query_node = CompiledQueryNode(id, parent_id, bound_vars, node, fragment, compiled_fragment)
    push!(compiled_query_nodes, compiled_query_node)
    for child in node.children
      compile_query_nodes(id, bound_vars, child, compiled_query_nodes)
    end
  end
  if typeof(node) == FixedNode
    for child in node.children
      compile_query_nodes(parent_id, parent_bound_vars, child, compiled_query_nodes)
    end
  end
end

function compile_node(node)
  wrapper = QueryNode(:session, [:session], [node])
  compiled_query_nodes = []
  compile_query_nodes(:root, Symbol[], wrapper, compiled_query_nodes)
  @show compiled_query_nodes
end

# --- interpreting ---
# dumb slow version just to get the logic right
# TODO not that

function interpret_value(value, bound_vars)
  if isa(value, StringExpr) 
    string((isa(v,Symbol) ? string(bound_vars[v]) : v for v in value.values)...)
  else
    string(value)
  end
end

function interpret_node(parent, node::TextNode, bound_vars, state)
  push!(parent.children, interpret_value(node.text, bound_vars))
end

function interpret_node(parent, node::AttributeNode, bound_vars, state)
  key = interpret_value(node.key, bound_vars)
  val = interpret_value(node.val, bound_vars)
  parent.attrs[key] = string(get(parent.attrs, key, ""), val) 
end

function interpret_node(parent, node::FixedNode, bound_vars, state)
  tag = Symbol(interpret_value(node.tag, bound_vars))
  child = Hiccup.Node(tag)
  push!(parent.children, child)
  for grandchild in node.children
    interpret_node(child, grandchild, bound_vars, state) 
  end
end

function interpret_node(parent, node::QueryNode, bound_vars, state)
  columns = state[node.table].columns
  @assert length(node.vars) == length(columns)
  for r in 1:length(columns[1])
    if all((!(var in keys(bound_vars)) || (bound_vars[var] == columns[c][r]) for (c, var) in enumerate(node.vars)))
      new_bound_vars = copy(bound_vars)
      for (c, var) in enumerate(node.vars)
        new_bound_vars[var] = columns[c][r]
      end
      for child in node.children
        interpret_node(parent, child, new_bound_vars, state)
      end
    end
  end
end

# --- plumbing ---

type View
  head::Any
  parsed_head::Node
  body::Any
  parsed_body::Node
  watchers::Set{Any}
end

function View() 
  View(nothing, TextNode(""), nothing, FixedNode("span", [TextNode("loading...")]), Set{Any}())
end

function set_head!(view::View, head)
  view.head = head
  view.parsed_head = parse_template(head)
  for watcher in view.watchers
    watcher()
  end
end

function set_body!(view::View, body)
  view.body = body
  view.parsed_body = parse_template(body)
  compile_node(view.parsed_body) # TODO temporary
  for watcher in view.watchers
    watcher()
  end
end

function Flows.watch(watcher, view::View)
  push!(view.watchers, watcher)
end

function render(window, view, world, session)
  @show :event_funs
  for event in world.events
    js(window, Blink.JSString("$event = imp_event(\"$event\")"), callback=false) # these never return! :(
  end
  head = Hiccup.Node(:head)
  interpret_node(head, view.parsed_head, Dict{Symbol, Any}(:session => session), world.state)
  body = Hiccup.Node(:body)
  interpret_node(body, view.parsed_body, Dict{Symbol, Any}(:session => session), world.state)
  @show :rendering head body
  @js_(window, diff.outerHTML(document.head, $(string(head))))
  @js_(window, diff.outerHTML(document.body, $(string(body))))
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
  load!(window, "src/diffhtml.js")
  load!(window, "src/Imp.js")
  @js_(window, session = $session)
  sleep(3) # :(
  handle(window, "event") do event
    refresh(world, Symbol(event["table"]), tuple(event["values"]...))
  end
  watch(world) do old_state, new_state
    render(window, view, world, session)
  end
  watch(view) do
    render(window, view, world, session)
  end
  refresh(world, :session, tuple(session))
  window
end

export View, set_head!, set_body!, window, watch

end
