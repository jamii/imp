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

typealias Value Union{StringExpr, Any} # where Any is converted to string

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

# --- interpreting ---
# dumb slow version just to get the logic right
# TODO not that

function interpret_value(value, bound_vars)
  if isa(value, StringExpr) 
    string((isa(v,Symbol) ? string(bound_vars[v]) : string(v) for v in value.values)...)
  else
    string(value)
  end
end

function interpret_node(parent, node::TextNode, bound_vars, state)
  push!(parent.children, interpret_value(node.text, bound_vars))
end

function interpret_node(parent, node::AttributeNode, bound_vars, state)
  parent.attrs[interpret_value(node.key, bound_vars)] = interpret_value(node.val, bound_vars)
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
  template::Any
  parsed_template::Node
  watchers::Set{Any}
end

function View() 
  template = quote [div] end
  View(template, parse_template(template), Set{Any}())
end

function set_template!(view::View, template)
  view.template = template
  view.parsed_template = parse_template(template)
  for watcher in view.watchers
    watcher()
  end
end

function Flows.watch(watcher, view::View)
  push!(view.watchers, watcher)
end

function render(window, view, world)
  @show :event_funs
  for event in world.events
    js(window, Blink.JSString("$event = imp_event(\"$event\")"), callback=false) # these never return! :(
  end
  root = Hiccup.Node(:div)
  interpret_node(root, view.parsed_template, Dict{Symbol, Any}(:session => "my session"), world.state)
  @show :rendering root
  @js_(window, diff.innerHTML(document.body, $(string(root))))
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

function window(world, view)
  window = Window()
  opentools(window)
  load!(window, "src/diffhtml.js")
  load!(window, "src/Imp.js")
  # watch_and_load(window, "src/Imp.js")
  sleep(3) # :(
  handle(window, "event") do event
    refresh(world, Symbol(event["table"]), tuple(event["values"]...))
  end
  watch(world) do old_state, new_state
    render(window, view, world)
  end
  watch(view) do
    render(window, view, world)
  end
  @js(window, document.body.innerHTML = $(string(Hiccup.Node(:div))))
  render(window, view, world)
  window
end

export View, set_template!, window, watch

end
