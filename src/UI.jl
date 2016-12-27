module UI

# Pkg.add("Blink")
# AtomShell.install()

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

pre = Sequence([
  @transient node(Id) => (Id, Int64, String) # (id) => (parent, ix, tag)
  @transient style(Id, String) => String
  @transient text(Id) => String
  @transient onclick(Id)
  @transient click(Id)
  @transient onkeydown(Id)
  @transient keydown(Id) => (Int64, String)
])

post = Sequence([
  # (level, parent, ix, id, tag)
  @transient sorted_node(Int64, Id, Int64, Id, String)

  @merge begin
    root = UI.root
    node(id) => (root, ix, tag)
    return sorted_node(1, root, ix, id, tag)
  end
  
  Fixpoint(
    @merge begin
      sorted_node(level, _, _, parent, _,)
      node(id) => (parent, ix, tag)
      return sorted_node(level+1, parent, ix, id, tag)
    end
  )
  
  @transient style_string(Id) => String
  
  @merge begin
    style(id, _) => _
    @query style(id, key) => val
    string = join(["$k: $v" for (k, v) in zip(key,val)], "; ")
    return style_string(id) => string
  end
])

@noinline function render(window, old_state, new_state)
  (removed, inserted) = Data.diff(old_state[:sorted_node], new_state[:sorted_node])
  (_, _, _, removed_id, _) = removed
  (_, parent, ix, id, tag) = inserted
  (_, (style_id, style)) = Data.diff(old_state[:style_string], new_state[:style_string])
  (_, (text_id, text)) = Data.diff(old_state[:text], new_state[:text])
  (_, (onclick,)) = Data.diff(old_state[:onclick], new_state[:onclick])
  (_, (onkeydown,)) = Data.diff(old_state[:onkeydown], new_state[:onkeydown])
  @show removed_id parent ix id tag style_id style text_id text onclick onkeydown
  # TODO remove old event handlers
  @js_(window, render($removed_id, $parent, $ix, $id, $tag, $style_id, $style, $text_id, $text, $onclick, $onkeydown))
end

function render(window, state)
  empty_state = Dict(name => empty(relation) for (name, relation) in state)
  render(window, empty_state, state)
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

function window(world)
  window = Window()
  opentools(window)
  watch_and_load(window, "src/Imp.js")
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

export render, window, Id, @id, root

end
