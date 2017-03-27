module LB

using Match
  
type Var 
  name::Symbol
  typ::Symbol
end

typealias Value Union{String, Symbol}

type Attribute
  key::Symbol
  val::Value
end

abstract Node

type TextNode <: Node
  text::Value
end

type FixedNode <: Node
  tag::Symbol
  attributes::Vector{Attribute}
  children::Vector{Node}
end

type QueryNode <: Node
  node::FixedNode
  table::Symbol
  vars::Vector{Var}
end

function preprocess(form)
  @match form begin
    Expr(:vect || :vcat || :hcat, args, _) => map(preprocess, args)
    Expr(head, args, _) => Expr(head, map(preprocess, args)...)
    args::Vector => args
    value::Union{Symbol, String, Number} => value
    _ => error("What are this? $form")
  end
end

function parse_node(form) 
  @match form begin
    text::String => TextNode(text)
    var::Symbol => TextNode(var)
    [tag::Symbol, args...] => begin
      (attributes, more_args) = parse_attributes(args)
      nodes = parse_nodes(more_args)
      FixedNode(tag, attributes, nodes)
    end
    _ => error("What are this? $form")
  end
end

function parse_attributes(forms)
  attributes = Attribute[]
  while true
    @match forms begin
      [Expr(:(=), [key::Symbol, val::Value], _), rest...] => begin
        push!(attributes, Attribute(key, val))
        forms = rest
      end
      _ => return (attributes, forms)
    end
  end
end

function parse_nodes(forms)
  @match forms begin
    [Expr(:call, [table::Symbol, vars...], _), child] => [QueryNode(parse_node(child), table, map(parse_var, vars))]
    _ => map(parse_node, forms)
  end
end
  
function parse_var(form)
  @match form begin
    Expr(::, [name::Symbol, typ::Symbol], _) => Var(name, typ)
    _ => error("What are this? $form")
  end
end

function parse_dom(form)
  @match form begin
    Expr(:block, [Expr(:line, _, _), child], _) => parse_node(preprocess(child))
    _ => parse_node(preprocess(form))
  end
end

d = quote 
[div
  chat(session:string)
  [div
    message(message:int, text:string, time:string)
    [div
      [span class="message-time" time]
      [span class="message-text" text]
    ]
  ]
]
end

parse_dom(d)

# foreach needs to be only child of parent

# id_1[session] = "1_" + int:string:convert(string:hash(session)) <-
#   id_0[session] = _,
#   chat(session).
# 
# dom:parent[child] = parent <-
#   id_1[session] = child,
#   id_0[session] = parent.
#   
# position_1(session, i; id) <-
#   seq<<>>
#   id_1[session] = id.
#   
# # i comes after parent vars, before new unique vars
# position_4(session, i; message, text, time, id) <-
#   seq<<>>
#   id_4[session, message, text, time] = id.
#   
# dom:position[session, id] = i <-
#   position_4(session, i; message, text, time, id).
#   
# # OR
# 
# dom:position[session, id] = 0 <-
#   id_5[session, message, text, time] = id.
#   
# # then  
#   
# dom:node(session, id),
# dom:attribute[session, id, "className"] = "message-time",
# dom:text[session, id] = time <-
#   id_5[session, message, text, time] = id.

# parse and convert into tree of fixed vs query
# with novel vars at each step
# spit out ids for each step, then parent, position
# spit out node, attribute, text

end
