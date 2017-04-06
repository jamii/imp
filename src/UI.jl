module UI

# Pkg.add("Blink")
# AtomShell.install()

using Data
# using Query
# using Flows
using Blink
using Match
using Hiccup

# --- ast ---

typealias Value Union{String, Symbol}

immutable Attribute
  key::Value
  val::Value
end

abstract Node

immutable TextNode <: Node
  text::Value
end

immutable FixedNode <: Node
  tag::Symbol
  attributes::Vector{Attribute}
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

function parse_nodes(exprs)
  map(parse_node, exprs)
end

function parse_node(expr) 
  @match expr begin
    text::Value => TextNode(text)
    Expr(:call, [table::Symbol, children_expr, vars...], _) => begin
      @match children_expr begin
        Expr(:->, [Expr(:tuple, [], _), Expr(:block, children_exprs, _)], _) => begin
          children = parse_nodes(filter((e) -> !is_line_number(e), children_exprs))
          QueryNode(table, vars, children)
        end
        _ => error("What are this? $children_expr")
      end
    end
    [tag::Symbol, args...] => begin
      (attributes, more_args) = parse_attributes(args)
      nodes = parse_nodes(more_args)
      FixedNode(tag, attributes, nodes)
    end
    _ => error("What are this? $expr")
  end
end

function is_line_number(expr)
  @match expr begin
    Expr(:line, _, _) => true
    _ => false
  end
end

function parse_attributes(exprs)
  attributes = Attribute[]
  while true
    @match exprs begin
      [Expr(:(=), [key::Value, val::Value], _), rest...] => begin
        push!(attributes, Attribute(key, val))
        exprs = rest
      end
      _ => return (attributes, exprs)
    end
  end
end

function parse_dom(expr)
  @match expr begin
    Expr(:block, [Expr(:line, _, _), child], _) => parse_node(preprocess(child))
    _ => parse_node(preprocess(expr))
  end
end

# --- interpreting ---
# dumb slow version just to get the logic right
# TODO not that

function interpret_value(value, bound_vars)
  string(isa(value, Symbol) ? bound_vars[value] : value)
end

function interpret_node(node::TextNode, bound_vars, data)
  return [interpret_value(node.text, bound_vars)]
end

function interpret_node(node::FixedNode, bound_vars, data)
  attributes = Dict{String, String}()
  for attribute in node.attributes
    attributes[interpret_value(attribute.key, bound_vars)] = interpret_value(attribute.val, bound_vars)
  end
  nodes = vcat([interpret_node(child, bound_vars, data) for child in node.children]...)
  return [Hiccup.Node(node.tag, attributes, nodes)]
end

function interpret_node(node::QueryNode, bound_vars, data)
  nodes = []
  columns = data[node.table].columns
  @assert length(node.vars) == length(columns)
  for r in 1:length(columns[1])
    if all((!(var in keys(bound_vars)) || (bound_vars[var] == columns[c][r]) for (c, var) in enumerate(node.vars)))
      new_bound_vars = copy(bound_vars)
      for (c, var) in enumerate(node.vars)
        new_bound_vars[var] = columns[c][r]
      end
      for child in node.children
        push!(nodes, interpret_node(child, new_bound_vars, data)...)
      end
    end
  end
  return nodes
end

d = quote
  [div
    login(session) do
      [input "type"="text" "placeholder"="What should we call you?"]
    end
    
    chat(session) do
      [div
        message(message, text, time) do
          [div
            [span "class"="message-time" time]
            [span "class"="message-text" text]
          ]
        end
      ]
      [input "type"="text" "placeholder"="What do you want to say?"]
    end
  ]
end

node = parse_dom(d)

data = Dict(
  :login => Relation(([],), 1),
  :chat => Relation((["my session"],), 1),
  :message => Relation(([0, 1], ["foo", "bar"], [11, 22]), 1)
  )
  
@show interpret_node(node, Dict{Symbol, Any}(:session => "my session"), data)[1]

# --- plumbing ---

# function watch_and_load(window, file)
#   load!(window, file)
#   @schedule begin
#     (waits, _) = open(`inotifywait -me CLOSE_WRITE $file`)
#     while true
#       readline(waits)
#       load!(window, file)
#     end
#   end
# end
# 
# function window(world)
#   window = Window()
#   opentools(window)
#   watch_and_load(window, "src/Imp.js")
#   sleep(3) # :(
#   handle(window, "event") do event
#     refresh(world, Symbol(event["table"]), tuple(event["values"]...))
#   end
#   watch(world) do old_state, new_state
#     render(window, old_state, new_state)
#   end
#   innerHTML = "<div id=\"$root\"></div>"
#   @js(window, document.body.innerHTML = $innerHTML)
#   render(window, world.state)
#   window
# end
# 
# export render, window, Id, @id, root

end
