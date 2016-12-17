module UI

# Pkg.add("Blink")
# AtomShell.install()
# Pkg.add("Hiccup")

using Data
using Flows
using Blink
using Hiccup

function event(table_name, values)
  Blink.jsexpr(quote 
    Blink.msg("event", d(("table", $table_name), ("values", $values)))
  end).s
end

macro event(expr)
  (name, keys, vals) = Data.parse_relation(expr)
  :(event($(string(name)), [$(map(esc, keys)...), $(map(esc, vals)...)]))
end

function render(window, outputs)
  html = string(Hiccup.div("#main", outputs[:window][1][1]))
  @js_(window, morphdom(document.getElementById("main"), $html))
end

function window(world)
  window = Window()
  loadjs!(window, "https://unpkg.com/morphdom@2.2.1/dist/morphdom-umd.js")
  sleep(3) # :(
  handle(window, "event") do event
    push!(world.inputs[symbol(event["table"])], tuple(event["values"]...))
    refresh(world)
  end
  watch(world) do old_outputs, new_outputs
    render(window, new_outputs)
  end
  @js window document.body.innerHTML = "<div id=\"main\"></div>"
  render(window, world.outputs)
  window
end

function hbox(nodes)
  Hiccup.div(Dict(:style => "display: flex; flex-direction: row"), nodes)
end

function vbox(nodes)
  Hiccup.div(Dict(:style => "display: flex; flex-direction: column"), nodes)
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

export event, @event, render, window, hbox, vbox

end
