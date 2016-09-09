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

function run(num_x, num_y, num_mines)
  srand(999)
  
  @relation state() = Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) = Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) = (Int64, Int64)
  @relation display() = Hiccup.Node
  
  @merge! state begin 
    s = :game_in_progress
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
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when nx in 1:num_x
      @when ny in 1:num_y
      @when (nx != x) || (ny != y)
      return (nx::Int64, ny::Int64)
    end
    
    @merge! state begin 
      num_cleared = length(@query begin
        cleared(x,y)
        return (x::Int64, y::Int64)
      end)
      @when num_cleared + num_mines >= num_x * num_y
      s = :game_won
      return (s::Symbol,)
    end
    
    @merge! state begin
      clicked($event_number, x, y)
      mine(x,y)
      s = :game_lost
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
         (:game_in_progress, _, true, 0) => button("_")
         (:game_in_progress, _, true, _) => button(string(count))
         (:game_in_progress, _, false, _) => button(Dict(:onclick => @event clicked(x,y)), "X")
         (_, true, _, _) => button("💣")
         (_, false, _, _) => button(string(count))
         _ => error()
       end
     end)
   end)
    
   Blink.body!(window, node)
    
  end
  
  (state, mine, mine_count, clicked, display, cleared)
end

(state, mine, mine_count, clicked, display, cleared) = run(10, 20, 10)

end
