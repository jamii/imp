module UI

# Pkg.add("Blink")
# AtomShell.install()
# Pkg.add("Hiccup")

using Data
using Flows
using Blink

# typealias Id UInt

# macro id(args...)
#   h = :(zero(UInt))
#   for arg in args
#     h = :(hash($(esc(arg)), $h))
#   end
#   h
# end

# root = UInt(0)

typealias Id String

macro id(args...)
  :(join([$(map(esc,args)...)], "-"))
end

root = "root"

# (id) => (parent, ix, kind, class, text)
pre = @transient node(Id) => (Id, Int64, String, String, String)

post = Sequence([
  # (level, parent, ix, id, kind, class, text)
  @transient sorted_node(Int64, Id, Int64, Id, String, String, String)

  @merge begin
    root = UI.root
    node(id) => (root, ix, kind, class, text)
    return sorted_node(1, root, ix, id, kind, class, text)
  end
  
  Fixpoint(
    @merge begin
      sorted_node(level, _, _, parent, _, _, _)
      node(id) => (parent, ix, kind, class, text)
      return sorted_node(level+1, parent, ix, id, kind, class, text)
    end
  )
])

function render(window, state)
  (_, id, parent, ix, kind, class, text) = state[:sorted_node].columns
  @js(window, render($id, $parent, $ix, $kind, $class, $text))
end

function window(world)
  window = Window()
  opentools(window)
  load!(window, "src/Imp.js")
  load!(window, "src/Imp.css")
  sleep(3) # :(
  handle(window, "event") do event
    @show event
    refresh(world, Symbol(event["table"]), tuple(event["values"]...))
  end
  watch(world) do old_state, new_state
    render(window, new_state)
  end
  innerHTML = "<div id=\"$root\"></div>"
  @js(window, document.body.innerHTML = $innerHTML)
  render(window, world.state)
  window
end

export render, window, @id, root

end
