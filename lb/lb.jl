module LB

using Match

# --- ast ---
  
struct Var 
  name::Symbol
  typ::Symbol
end

typealias Value Union{String, Symbol}

struct Attribute
  key::Value
  val::Value
end

abstract Node

struct TextNode <: Node
  text::Value
end

struct FixedNode <: Node
  tag::Symbol
  attributes::Vector{Attribute}
  children::Vector{Node}
end

struct QueryNode <: Node
  children::Vector{Node}
  table::Symbol
  vars::Vector{Var}
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
    Expr(:call, [table::Symbol, children_expr, var_exprs...], _) => begin
      @match children_expr begin
        Expr(:->, [Expr(:tuple, [], _), Expr(:block, children_exprs, _)], _) => begin
          children = parse_nodes(filter((e) -> !is_line_number(e), children_exprs))
          vars = map(parse_var, var_exprs)
          QueryNode(children, table, vars)
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
  
function parse_var(expr)
  @match expr begin
    Expr(::, [name::Symbol, typ::Symbol], _) => Var(name, typ)
    _ => error("What are this? $expr")
  end
end

function parse_dom(expr)
  @match expr begin
    Expr(:block, [Expr(:line, _, _), child], _) => parse_node(preprocess(child))
    _ => parse_node(preprocess(expr))
  end
end

# --- codegen ---

function var_string(var::Var) 
  @match var.typ begin
    :int => "int:string:convert[$(var.name)]"
    :string => "int:string:convert[string:hash[$(var.name)]]"
    _ => error("What type are this? $(var)")
  end
end

function val_string(val::Value)
  @match val begin
    _::String => "\"$val\""
    _::Symbol => "$val"
  end
end

# fixed/query is nearest FixedNode/QueryNode ancestor
function generate(node, id, fixed_id, query_id, fixed_vars, query_vars, output)
  vars = @match node begin
    _::QueryNode => vcat(query_vars, [var for var in node.vars if !(var in query_vars)])
    _ => query_vars
  end
  
  var_names = join(["$(var.name)" for var in vars], ",")
  var_types = join(["$(var.typ)($(var.name))" for var in vars], ",")
  var_strings = join([var_string(var) for var in vars], " + \"/\" + ")
  fixed_var_names = join(["$(var.name)" for var in fixed_vars], ",")
  fixed_var_types = join(["$(var.typ)($(var.name))" for var in fixed_vars], ",")
  query_var_names = join(["$(var.name)" for var in query_vars], ",")
  
  @match node begin
    _::TextNode => begin
      # TODO can we make a text node in the dom instead of using dom:text?
      text_data = "dom:text[session, id] = $(val_string(node.text)) <-\n    id_$(fixed_id)[$(fixed_var_names)] = id"
      push!(output, text_data)
      return "$(start) + 1"
    end
    _::QueryNode => begin
      query_string = string(Expr(:call, table, [var.name for var in vars]...))
      query_type = "data_$(id)[$(var_names)] ->\n    $(var_types)."
      query_data = "data_$(id)[$(var_names)] <-\n    $(query_string), data_$(query_id)[$(query_var_names)] = _."
      push!(output, query_type, query_data)
      for (i, child) in enumerate(node.children)
        generate(child, "$(id)_$(i)", fixed_id, id, fixed_vars, vars, output)
      end
    end
    _::FixedNode => begin
      id_type = "id_$(id)[$(var_names)] = id ->\n    $(var_types), string(id)."
      id_data = "id_$(id)[$(var_names)] = \"$(id)/\" + $(var_strings) <-\n    data_$(query_id)[$(query_var_names)] = _."
      parent_data = "dom:parent[session, child] = parent <-\n    id_$(id)[$(var_names)] = child, id_$(fixed_id)[$(fixed_var_names)] = parent."

      node_data = "dom:node(session, id)"
      tag_data = "dom:tag[session, id] = \"$(node.tag)\""
      attribute_data = ["dom:attribute[session, id, $(val_string(attribute.key))] = $(val_string(attribute.val))" for attribute in node.attributes]
      dom_data_head = join(vcat([node_data], [tag_data], attribute_data), ",\n")
      dom_data = "$(dom_data_head) <-\n    id_$(id)[$(var_names)] = id."
      
      push!(output, id_type, id_data, parent_data, dom_data)
      
      for (i, child) in enumerate(node.children)
        generate(child, "$(id)_$(i)", id, query_id, vars, query_vars, output)
      end
    end
  end
end

# TODO figure out positioning! 
# hand on, maybe threading last does work?
# parent calculates position of children
# fixed node pass start = last(prev) + 1, sets position for fixed children, returns last = start + 1
# query node positions each fixed child at start + mult * row + offset, returns last = start + num_children * num_rows

function generate(node)
  output = String[]
  
  generate(node, "lb", "lb", "lb", Var[Var(:session, :string)], Var[Var(:session, :string)], output)
  
  unshift!(output, 
    "data_lb[session] -> string(session)",
    "data_lb[session] <- dom:session(session)",
    "id_lb[session] = id -> string(session), string(id).", 
    "id_lb[session] = \"root\" <- dom:session(session).")
  
  join(output, "\n\n")
end
  
  push!(output, parent)
  
  @match query begin
    QueryNode(_, _, _) => begin
      position_var_names = join(["$(var.name)" for var in vcat(bound_vars, [Var(:pos, :int)], free_vars, [Var(:id, :int)])], ",")
      position_type = "position_$(id)($(position_var_names)) ->\n    $(var_types), int(pos), string(id)."
      position_data = "position_$(id)($(position_var_names)) <-\n    seq<<>> id_$(id)[$(var_names)] = id."
      position = "dom:position[session, id] = pos <-\n    position_$(id)($(position_var_names))."
      push!(output, position_type, position_data, position)
    end
    _ => begin
      position = "dom:position[session, id] = $(child_pos-1) <-\n    id_$(id)[$(var_names)] = id."
      push!(output, position)
    end
  end
  
  
  push!(output, dom_data)
end

function compile(expr)
  generate(parse_dom(expr))
end

# --- demos ---

# TODO 
# add extra positions so that can have multiple children
# figure out how to handle styles (maybe k:v)

# ALTERNATIVE
# generate code to create an api
# treat as tree of query nodes, mapped to dom
# diff query tree and patch up dom

chat = 
"""
block(`chat) {
  alias(`ui:dom, `dom),
  
  clauses(`{
    // state 
    
    text[message] = text ->
      int(message), string(text).
      
    session[message] = session ->
      int(message), string(session).
      
    time[message] = time ->
      int(message), datetime(time).
      
    input_text[session] = text ->
      string(session), string(text).
      
    user_name[session] = user_name ->
      string(session), string(user_name).
      
    input_user_name[session] = user_name ->
      string(session), string(user_name).
      
    // computed
    
    max_message[] = max_message <-
      agg<<max_message = max(message)>>
      text[message] = _.
    
    max_message[] = 0 <-
      !text[_] = _.
      
    // event handlers
    
    ^input_user_name[session] = text <-
      +dom:change[session, "user-name"] = text.
      
    +user_name[session] = input_user_name[session] <-
      +dom:key_down[session, "user-name"] = 13, // ENTER
      input_user_name[session] != "".
    
    ^input_text[session] = text <-
      +dom:change[session, "new-message-entry"] = text.
    
    +text[message] = input_text[session],
    +session[message] = session,
    +time[message] = datetime:now[],
    +dom:clear(session, "new-message-entry"),
    +dom:scroll_into_view(session, "message-" + int:string:convert[message]) <-
      +dom:key_down[session, "new-message-entry"] = 13, // ENTER
      input_text[session] != "",
      message = max_message@prev[] + 1.
      
    // ui
    
    login(session) <-
      dom:url_fragment[session] = "#chat",
      !user_name[session] = _.
  
    chat(session) <-
      dom:url_fragment[session] = "#chat".
      // user_name[session] = _.
      
    message(message, text[message], datetime:string:convert[time[message]]).
    
$(compile(quote
  [div
    login(session:string) do
      [input "type"="text" "placeholder"="What should we call you?"]
    end
    
    chat(session:string) do
      [div
        message(message:int, text:string, time:string) do
          [div
            [span "class"="message-time" time]
            [span "class"="message-text" text]
          ]
        end
      ]
      [input "type"="text" "placeholder"="What do you want to say?"]
    end
  ]
end))
    
  })
} <-- .
"""

open("lb/demo/chat.logic", "w") do f
  write(f, chat)
end

run(`bash ./lb/build.sh`)

  [div
    login(session:string) do
      [input "type"="text" "placeholder"="What should we call you?"]
    end
    
    chat(session:string) do
      [div
        message(message:int, text:string, time:string) do
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
