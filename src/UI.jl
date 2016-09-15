module UI

# Pkg.add("Blink")
# Blink.AtomShell.install()
# Pkg.add("Hiccup")

using Blink
using Hiccup

function event(table_name, values)
  Blink.jsexpr(quote 
    Blink.msg("event", d(("table", $table_name), ("values", $values)))
  end).s
end

macro event(expr)
  @assert expr.head == :call
  :(event($(string(expr.args[1])), [$(map(esc, expr.args[2:end])...)]))
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
  :(Window($(esc(flow)), Dict($([:($(string(table)) => $(esc(table))) for table in event_tables]...))))
end

function hbox(nodes)
  Hiccup.div(nodes)
end

function vbox(nodes)
  Hiccup.div(nodes)
end

function Base.cmp{T1, T2}(n1::Hiccup.Node{T1}, n2::Hiccup.Node{T2})
  c = cmp(T1, T2)
  if c != 0; return c; end
  c = cmp(length(n1.attrs), length(n2.attrs))
  if c != 0; return c; end
  for (a1, a2) in zip(n1.attrs, n2.attrs)
    c = cmp(a1, a2)
    if c != 0; return c; end
  end
  c = cmp(length(n1.children), length(n2.children))
  if c != 0; return c; end
  for (c1, c2) in zip(n1.children, n2.children)
    c = cmp(c1, c2)
    if c != 0; return c; end
  end
  return 0
end

function Base.isless(n1::Hiccup.Node, n2::Hiccup.Node)
  cmp(n1, n2) == -1
end

export event, @event, Window, @Window, hbox, vbox

end
