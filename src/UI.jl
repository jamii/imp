module UI

# Pkg.add("Blink")
# AtomShell.install()
# Pkg.add("Hiccup")

using Data
using Query
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
  @transient sorted_node(Int64, Id, Int64, Id, String)

  @merge begin
    root = UI.root
    node(id) => (root, ix, kind, _, _)
    return sorted_node(1, root, ix, id, kind)
  end
  
  Fixpoint(
    @merge begin
      sorted_node(level, _, _, parent, _,)
      node(id) => (parent, ix, kind, _, _)
      return sorted_node(level+1, parent, ix, id, kind)
    end
  )
  
  @transient class(Id, String)
  
  @merge begin
    node(id) => (_, _, _, class, _)
    return class(id, class)
  end
  
  @transient text(Id, String)
  
  @merge begin
    node(id) => (_, _, _, _, text)
    return text(id, text)
  end
])

function render(window, old_state, new_state)
  (removed, inserted) = Data.diff(old_state[:sorted_node], new_state[:sorted_node])
  (_, _, _, removed_id, _) = removed
  (_, parent, ix, id, kind) = inserted
  (_, (class_id, class)) = Data.diff(old_state[:class], new_state[:class])
  (_, (text_id, text)) = Data.diff(old_state[:text], new_state[:text])
  @js(window, render($removed_id, $parent, $ix, $id, $kind, $class_id, $class, $text_id, $text))
end

function render(window, state)
  (_, parent, ix, id, kind) = state[:sorted_node].columns
  (class_id, class) = state[:class].columns
  (text_id, text) = state[:text].columns
  @js(window, render($([]), $parent, $ix, $id, $kind, $class_id, $class, $text_id, $text))
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
    render(window, old_state, new_state)
  end
  innerHTML = "<div id=\"$root\"></div>"
  @js(window, document.body.innerHTML = $innerHTML)
  render(window, world.state)
  window
end

export render, window, @id, root

end
