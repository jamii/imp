module Table

using Data
using Query
using UI
using Match
using Blink
using Hiccup
@tags button


function debug(relation)
  @relation picked(Int64) => Symbol
  
  @Window(picked) do window, event_number

    header = @query begin
      relation(name, _)
      node = button(Dict(:onclick => @event(picked() => name)), string(name))
      return (node::Node,)
    end
    
    Blink.body!(window, hbox(header[1]))
    
  end
  
  (picked,)
  
end

import Minesweeper
minesweeper_relations = Minesweeper.run(30, 30, 100)
relation = Relation(([:state, :mine, :mine_count, :cleared, :clicked, :cell, :row, :grid], collect(minesweeper_relations)), 1)
(picked,) = debug(relation)

end
