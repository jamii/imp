module UI

# Pkg.add("Blink")
# Blink.AtomShell.install()
# Pkg.add("Hiccup")

using Blink
using Hiccup
@tags button

function event(table_name, values)
  Blink.jsexpr(quote 
    Blink.msg("event", d(("table", $table_name), ("values", $values)))
  end).s
end

macro event(expr)
  assert(expr.head == :call)
  :(event($(string(expr.args[1])), [$(expr.args[2:end]...)]))
end

function Blink.Window(flow, event_tables)
  w = Window()
  event_number = 1
  handle(w, "event") do args 
    values = args["values"]
    insert!(values, 1, event_number)
    push!(event_tables[args["table"]], values)
    flow(w, event_number)
    event_number += 1
  end
  flow(w, 0)
  w
end

macro Window(flow, event_tables...)
  :(Window($flow, Dict($([:($(string(table)) => $table) for table in event_tables]...))))
end

# using Data
# clicked = Relation((Int64[], String[]))
# @Window(clicked) do w, event_number
#   body!(w, button("#my_button", Dict(:onclick => @event clicked("my_button")), "clicked $event_number times"))
# end

# function ids(node, acc)
#   if isa(node, Node)
#     if haskey(node.attrs, :id)
#       push!(acc, node.attrs[:id])
#     end
#     for child in node.children
#       ids(child, acc)
#     end
#   end
#   acc
# end
# 
# function ids(node)
#   ids(node, Set{String}())
# end

export event, @event, Window, @Window

end
