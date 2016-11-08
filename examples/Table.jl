module Table

using Data
using Query
using UI
using Match
using Blink
using Hiccup
@tags button

macro exists(clause)
  quote 
    exists = @query begin 
      exists = true
      $clause
      return (exists::Bool,)
    end
    length(exists[1]) == 1
  end
end

function debug(relation)
  @relation displaying() => (Int64, String)
  
  @query return displaying() => (0, relation[1][1])
  
  window = @Window(displaying) do window, event_number
    
    header = @query begin
      relation(name) => _
      displayed = @exists displaying() => (_, $name)
      style = displayed ? "font-weight: bold" : ""
      node = button(Dict(:onclick => @event(displaying() => name), :style => style), name)
      return (name::String,) => node::Node
    end
    
    grid = @query begin
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      style = "margin-left: 2em"
      node = vbox(map((v) -> Hiccup.div(Dict(:style=>style), string(v)), relation[c]))
      return (c::Int64,) => node::Node
    end
    
    Blink.body!(window, vbox([hbox(header[2]), hbox(grid[2])]), fade=false)
    
  end
  
  tick_every(window, 1000)
end

import Minesweeper
minesweeper_relations = Minesweeper.run(30, 30, 100)
relation = Relation((["state", "mine", "mine_count", "cleared", "clicked", "cell", "row", "grid"], collect(minesweeper_relations)), 1)
debug(relation)

end
