module Minesweeper

using Data
using Query
using UI
using Match

function run(num_x, num_y, num_mines)
  @relation state() => Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) => Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) => Int64, Int64
  
  @query begin 
    + state() = :game_ok
  end
  
  while length(mine) < num_mines
    @query begin 
      x = rand(1:num_x)
      y = rand(1:num_y)
      + mine(x,y)
    end 
  end
  
  @query begin 
    x in 1:num_x
    y in 1:num_y
    c = count(
      nx in -1:1
      ny in -1:1
      @when (nx != 0) || (ny != 0)
      mine(x+nx, y+ny) = true
    )
    + mine_count(x, y) = c
  end
  
  @Window(clicked) do display, event_number
    @query begin
      clicked($event_number) = (x, y)
      + cleared(x, y)
    end
    
    fix!(cleared) do
      @query begin 
        cleared(x,y)
        mine_count(x,y,0)
        nx in -1:1
        ny in -1:1
        @when (nx * ny == 0) && (nx + ny != 0) # no boolean xor :(
        + cleared(x+nx, y+ny)
      end)
    end
    
    @query 
      clicked($event_number) = (x, y)
      mine(x,y)
      + state() = :game_over
    end
    
    @query begin 
      state() = state
      x in 1:num_x
      y in 1:num_y
      cleared = exists(cleared(x,y))
      mine = exists(mine(x,y))
      mine_count(x,y,count)
      node = @match (state, mine, cleared, count) begin
        (:game_over, true, _, _) => button("💣")
        (:game_over, false, _, _) => button(string(count))
        (:game_ok, _, true, 0) => button(" ")
        (:game_ok, _, true, _) => button(string(count))
        (:game_ok, _, false, _) => button("X", :onclick => @event clicked(x,y))
      end
      @group y node = h_box(node)
      @group x node = v_box(node)
      + display() = node
    end
    
  end
end
