module Minesweeper

using Data
using Query
using UI
using Match

function fix!(flow, relation)
  # TODO semi-naive
  while true 
    if union!(relation, flow())
      break;
    end
  end
end

function run(num_x, num_y, num_mines)
  state = @query([s::symbol],
  begin
    s = :game_ok
  end)
  
  mine = @query([x::Int64, y::Int64, m::Bool],
  begin 
    x in 1:num_x
    y in 1:num_y
    m = false
  end)
  
  union!(mine, @query([x::Int64, y::Int64, m::Bool],
  begin 
    i in 1:num_mines
    x = rand(1:num_x)
    y = rand(1:num_y)
    m = true
  end)) # TODO check for dupes
  
  mine_count = @query([x::Int64, y::Int64, c::Int64],
  begin 
    x in 1:num_x
    y in 1:num_y
    c = 0
  end),

  union!(mine_count, @query([x::Int64, y::Int64],
  begin 
    x in 1:num_x
    y in 1:num_y
    nx in -1:1
    ny in -1:1
    @when (nx != 0) || (ny != 0)
    mine(x+nx, y+ny, true)
  end))
  
  click = Relation(Int64[], Int64[], Int64[])
  cleared = @query([x::Int64, y::Int64, c::Bool],
  begin
    x in 1:num_x
    y in 1:num_y
    c = false
  end)
  
  @Window(click) do window, event_number
    
    union!(cleared, @query([x::Int64, y::Int64],
    begin
      event_number = event_number # TODO need a nicer way to ref external variables
      click(event_number, x, y)
    end))
    
    fix!(cleared) do
      @query([new_x::Int64, new_y::Int64, c::Bool],
      begin 
        cleared(x,y,true)
        mine_count(x,y,0)
        nx in -1:1
        ny in -1:1
        @when (nx * ny == 0) && (nx + ny != 0) # no boolean xor :(
        new_x = x + nx
        new_y = y + ny
        c = true
      end)
    end
    
    union!(state, @query([s::Symbol],
    begin
      event_number = event_number # TODO need a nicer way to ref external variables
      clicked(event_number, x, y)
      mine(x,y,true)
      s = :game_over
    end))
    
    display = @query([x::Int64, y::Int64, node],
    begin 
      state(state)
      x in 1:num_x
      y in 1:num_y
      cleared(x,y,cleared)
      mine(x,y,mine)
      mine_count(x,y,count)
      node = @match (state, mine, cleared, count) begin
        (:game_over, true, _, _) => button("", "💣")
        (:game_over, false, _, _) => button("", string(count))
        (:game_ok, _, true, 0) => button("", " ")
        (:game_ok, _, true, _) => button("", string(count))
        (:game_ok, _, false, _) => button("", Dict(:onclick => @event click(x,y)), "X")
      end
    end)
    
    # TODO need better aggregates for display
    # TODO need some form of ordering
    
    display = @query([y::Int64],
    ([], push_exp!, node),
    begin
      display(x,y,node)
    end)
    
    display = @query([],
    ([], push_exp!, node),
    begin 
      display(y, nodes)
      node = h_box(nodes)
    end)
    
    node = v_box(display.columns[1][1])
    
    body!(window, node)
      
  end
end
