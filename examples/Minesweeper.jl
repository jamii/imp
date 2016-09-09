module Minesweeper

using Data
using Query
using UI
using Match
using Blink
using Hiccup
@tags button

function exists(relation)
  length(relation) > 0
end

function fix!(flow, relation)
  while true
    new_relation = merge(relation, flow())
    if new_relation.columns == relation.columns
      return
    end
    replace!(relation, new_relation)
  end
end

macro fix!(relation, flow)
  :(fix!((() -> @query $(esc(flow))), $(esc(relation))))
end

macro merge!(relation, query)
  :(merge!($(esc(relation)), (@query $(esc(query)))))
end

function hbox(nodes)
  Hiccup.div(nodes)
end

function vbox(nodes)
  Hiccup.div(nodes)
end

macroexpand(quote
@merge! mine_count begin 
  x in 1:num_x
  y in 1:num_y
  neighbouring_mines = @query begin
    nx in (x-1):(x+1)
    ny in (y-1):(y+1)
    @when (nx != x) || (ny != y)
    mine(nx, ny) 
    return (nx::Int64, ny::Int64)
  end
  c = length(neighbouring_mines)
  return (x::Int64, y::Int64, c::Int64)
end
end)

function run(num_x, num_y, num_mines)
  srand(999)
  
  @relation state() = Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) = Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) = (Int64, Int64)
  @relation display() = Hiccup.Node
  
  @merge! state begin 
    s = :game_ok
    return (s::Symbol,)
  end
  
  @fix! mine begin 
    mines = @query begin
      mine(x, y)
      return (x::Int64, y::Int64)
    end
    @when length(mines) < num_mines
    x = rand(1:num_x)
    y = rand(1:num_y)
    return (x::Int64, y::Int64)
  end 
  
  @merge! mine_count begin 
    x in 1:num_x
    y in 1:num_y
    neighbouring_mines = @query begin
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when (nx != x) || (ny != y)
      mine(nx, ny) 
      return (nx::Int64, ny::Int64)
    end
    c = length(neighbouring_mines)
    return (x::Int64, y::Int64, c::Int64)
  end
  
  Window(Dict("clicked" => clicked)) do window, event_number
    
    @merge! cleared begin
      clicked($event_number, x, y)
      return (x::Int64, y::Int64)
    end
    
    @fix! cleared begin 
      cleared(x,y)
      mine_count(x,y,0)
      pos in [(x-1,y), (x+1, y), (x, y-1), (x, y+1)]
      nx = pos[1]
      ny = pos[2]
      return (nx::Int64, ny::Int64)
    end
    
    @merge! state begin
      clicked($event_number, x, y)
      mine(x,y)
      s = :game_over
      return (s::Symbol,)
    end
    
    node = vbox(map(1:num_y) do y
      return hbox(map(1:num_x) do x  
        current_state = state.columns[1][1]
        is_cleared = exists(@query begin 
          cleared($x,$y) 
          e = true
          return (e::Bool,)
        end)
        is_mine = exists(@query begin 
          mine($x,$y) 
          e = true 
          return (e::Bool,)
        end)
        count = (@query begin 
          mine_count($x,$y,count)
          return (count::Int64,)
        end).columns[1][1]
        return @match (current_state, is_mine, is_cleared, count) begin
         (:game_over, true, _, _) => button("💣")
         (:game_over, false, _, _) => button(string(count))
         (:game_ok, _, true, 0) => button("_")
         (:game_ok, _, true, _) => button(string(count))
         (:game_ok, _, false, _) => button(Dict(:onclick => @event clicked(x,y)), "X")
         _ => error()
       end
     end)
   end)
    
   Blink.body!(window, node)
    
  end
  
  (state, mine, mine_count, clicked, display, cleared)
end

(state, mine, mine_count, clicked, display, cleared) = run(10, 20, 1)

end
