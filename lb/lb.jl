module LB

using Match
  
immutable Var 
  name::Symbol
  typ::Symbol
end

typealias Value Union{String, Symbol}

immutable Attribute
  key::Symbol
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
  node::FixedNode
  table::Symbol
  vars::Vector{Var}
end

function preprocess(expr)
  @match expr begin
    Expr(:vect || :vcat || :hcat, args, _) => map(preprocess, args)
    Expr(head, args, _) => Expr(head, map(preprocess, args)...)
    args::Vector => args
    value::Union{Symbol, String, Number} => value
    _ => error("What are this? $expr")
  end
end

function parse_node(expr) 
  @match expr begin
    [tag::Symbol, args...] => begin
      (attributes, more_args) = parse_attributes(args)
      nodes = parse_nodes(more_args)
      FixedNode(tag, attributes, nodes)
    end
    _ => error("What are this? $expr")
  end
end

function parse_attributes(exprs)
  attributes = Attribute[]
  while true
    @match exprs begin
      [Expr(:(=), [key::Symbol, val::Value], _), rest...] => begin
        push!(attributes, Attribute(key, val))
        exprs = rest
      end
      _ => return (attributes, exprs)
    end
  end
end

function parse_nodes(exprs)
  @match exprs begin
    [text::Value] => [TextNode(text)]
    [Expr(:call, [table::Symbol, vars...], _), child] => [QueryNode(parse_node(child), table, map(parse_var, vars))]
    _ => map(parse_node, exprs)
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

function walk(node, bound_vars, parent_id, child_pos, output)
  node_id = "$(parent_id)_$(child_pos)"
  @match node begin
    TextNode(text) => nothing # handled in generate_node on the parent
    FixedNode(tag, attributes, children) => begin
      generate_node(node, nothing, parent_id, node_id, child_pos, bound_vars, Var[], output)
      for (i, child) in enumerate(children)
        walk(child, bound_vars, node_id, i, output)
      end
    end
    QueryNode(FixedNode(tag, attributes, children), table, vars) => begin
      free_vars = Var[var for var in vars if !(var in bound_vars)]
      generate_node(node.node, node, parent_id, node_id, child_pos, bound_vars, free_vars, output)
      new_bound_vars = vcat(bound_vars, free_vars)
      for (i, child) in enumerate(children)
        walk(child, new_bound_vars, node_id, i, output)
      end
    end
  end
end

function walk(dom)
  output = String[]
  walk(dom, Var[Var(:session, :string)], "lb", 0, output)
  
  unshift!(output, 
    "id_lb[session] = id -> string(session), string(id).", 
    "id_lb[session] = \"root\" <- dom:session(session).")
  
  join(output, "\n\n")
end

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

function generate_node(node, query, parent_id, node_id, child_pos, bound_vars, free_vars, output)
  bound_var_names = join(["$(var.name)" for var in bound_vars], ",")
  free_var_names = join(["$(var.name)" for var in free_vars], ",")
  
  vars = vcat(bound_vars, free_vars)
  var_names = join(["$(var.name)" for var in vars], ",")
  var_types = join(["$(var.typ)($(var.name))" for var in vars], ",")
  var_strings = join([var_string(var) for var in vars], " + \"/\" + ")
  
  query_string = @match query begin
    QueryNode(_, table, vars) => string(Expr(:call, table, [var.name for var in vars]...), ",")
    _ => ""
  end
  
  id_type = "id_$(node_id)[$(var_names)] = id ->\n    $(var_types), string(id)."
  id_data = "id_$(node_id)[$(var_names)] = \"$(node_id)/\" + $(var_strings) <-\n    $(query_string) id_$(parent_id)[$(bound_var_names)] = _."
  push!(output, id_type, id_data)
  
  parent = "dom:parent[session, child] = parent <-\n    id_$(node_id)[$(var_names)] = child, id_$(parent_id)[$(bound_var_names)] = parent."
  push!(output, parent)
  
  @match query begin
    QueryNode(_, _, _) => begin
      position_var_names = join(["$(var.name)" for var in vcat(bound_vars, [Var(:pos, :int)], free_vars, [Var(:id, :int)])], ",")
      position_type = "position_$(node_id)($(position_var_names)) ->\n    $(var_types), int(pos), string(id)."
      position_data = "position_$(node_id)($(position_var_names)) <-\n    seq<<>> id_$(node_id)[$(var_names)] = id."
      position = "dom:position[session, id] = pos <-\n    position_$(node_id)($(position_var_names))."
      push!(output, position_type, position_data, position)
    end
    _ => begin
      position = "dom:position[session, id] = $(child_pos-1) <-\n    id_$(node_id)[$(var_names)] = id."
      push!(output, position)
    end
  end
  
  node_data = ["dom:node(session, id)"]
  attribute_data = ["dom:attribute[session, id, \"$(attribute.key)\"] = $(val_string(attribute.val))" for attribute in node.attributes]
  text_data = @match node.children begin
    [TextNode(text)] => ["dom:text[session, id] = $(val_string(text))"]
    _ => []
  end
  dom_data_head = join(vcat(node_data, attribute_data, text_data), ",\n")
  dom_data = "$(dom_data_head) <-\n    id_$(node_id)[$(var_names)] = id."
  push!(output, dom_data)
end

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
    
$(walk(parse_dom(quote 
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
end)))
    
  })
} <-- .
"""

open("lb/demo/chat.logic", "w") do f
  write(f, chat)
end

run(`bash ./lb/build.sh`)

end
