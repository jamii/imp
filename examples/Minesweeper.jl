module Minesweeper

using Data
using Query
using Flows
using UI
using Match
using Blink

world = World()

world[:state] = Relation(([:game_in_progress],), 0)
world[:mine] = Relation((Int64[], Int64[]), 2)
world[:cleared] = Relation((Int64[], Int64[]), 2)

world[:test] = Relation((Int64[1,2,3], Any[4,5,6], String["a","b","c"]), 1)

num_x = 30
num_y = 30
num_mines = 90

begin 
  setflow(world, Sequence([
    @stateful state() => Symbol
    @stateful mine(Int64, Int64)
    @stateful mine_count(Int64, Int64) => Int64
    @stateful cleared(Int64, Int64)
    
    @stateful grid(Id) => (Int64, Int64)
    
    @transient mine_attempts() => Int64
    @merge return mine_attempts() => 0
    Fixpoint(
      @merge begin
        @query mine(x, y)
        @when length(x) < num_mines
        nx = rand(1:num_x)
        ny = rand(1:num_y)
        mine_attempts() => a
        return mine(nx, ny)
        return mine_attempts() => a+1
      end
    )
    
    @merge begin
      x in 1:num_x
      y in 1:num_y
      @query begin
        nx in (x-1):(x+1)
        ny in (y-1):(y+1)
        @when (nx != x) || (ny != y)
        mine(nx, ny) 
        return (nx, ny)
      end
      return mine_count(x, y) => length(nx)
    end
    
    # @merge begin
    #   click(id)
    #   grid(id) => (x, y)
    #   return cleared(x, y)
    # end
    
    Fixpoint(
      @merge begin
        cleared(x,y)
        # @query begin mine(x,y); is_mine = true end
        # @when isempty(is_mine)
        mine_count(x,y) => 0
        nx in (x-1):(x+1)
        ny in (y-1):(y+1)
        @when nx in 1:num_x
        @when ny in 1:num_y
        @when (nx != x) || (ny != y)
        return cleared(nx, ny)
      end
    )
    
    @merge begin 
      @query cleared(x,y)
      num_cleared = length(x)
      @when num_cleared + num_mines >= num_x * num_y
      return state() => :game_won
    end 
    
    # @merge begin
    #   click(id)
    #   grid(id) => (x,y)
    #   mine(x,y)
    #   return state() => :game_lost
    # end
    
    # @merge begin 
    #   return node(@id(:grid)) => (@id(:root), 1, "div")
    #   return style(@id(:grid), "display") => "flex"
    #   return style(@id(:grid), "flex-direction") => "column"
    # end
    # 
    # @merge begin 
    #   x in 1:num_x
    #   return node(@id(:grid, x)) => (@id(:grid), x, "div")
    #   return style(@id(:grid, x), "display") => "flex"
    #   return style(@id(:grid, x), "flex-direction") => "row"
    # end
    # 
    # @merge begin
    #   state() => current_state
    #   x in 1:num_x
    #   y in 1:num_y
    #   @query begin cleared(x,y); is_cleared = true; end
    #   @query begin mine(x,y); is_mine = true; end
    #   mine_count(x, y) => count
    #   text = @match (current_state, !isempty(is_mine), !isempty(is_cleared), count) begin
    #     (:game_in_progress, _, true, 0) => "_"
    #     (:game_in_progress, _, true, _) => string(count)
    #     (:game_in_progress, _, false, _) => ""
    #     (:game_won, true, _, _) => "💣"
    #     (:game_lost, true, _, _) => "☀"
    #     (_, false, _, _) => string(count)
    #     other => error("The hell is this: $other")
    #   end
    #   return grid(@id(:grid, x, y)) => (x, y)
    #   return node(@id(:grid, x, y)) => (@id(:grid, x), y, "button")
    #   return text(@id(:grid, x, y)) => text
    #   return onclick(@id(:grid, x, y))
    #   return style(@id(:grid, x, y), "width") => "2em"
    #   return style(@id(:grid, x, y), "height") => "2em"
    # end
  ]))
end

w = window(world)

end
