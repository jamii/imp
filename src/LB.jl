module LB

using Blink

function lb(command)
  write("/tmp/script.lb", command)
  output = readstring(`/home/jamie/logicblox/bin/lb /tmp/script.lb`)
  raw_results = matchall(r"/--------------- _ ---------------\\([^$\\]*)\\--------------- _ ---------------/"s, output)
  stripped_results = [string("[", raw_result[37:(length(raw_result)-36)], "]") for raw_result in raw_results]
  @show stripped_results
  results = Any[eval(parse(stripped_result)) for stripped_result in stripped_results]
  @show results
end

begin 
  # run(`/home/jamie/logicblox/bin/lb compile project ./imp.project`)
  lb("""
    open imp
    addproject /home/jamie/imp
  """)
end

function col(matrix, i)
  (matrix == nothing || matrix == []) ? Any[] : matrix[:,i]
end
  

function render(window, event_name, event_values) 
  event_values_string = join(map(repr, event_values), ",")
  event_string = "+$event_name($event_values_string)."
  @show event_string
  command = """
    open imp
    
    query '_(id) <-
      node(id; _, _, _).
    '  
    
    exec '
      $event_string
    '
    
    query '_(l, parent, ix, id, tag) <-
      node(id; parent, ix, tag),
      level(id; l).
    '    
    
    query '_(id, k, v) <-
      style(id, k; v).
    ' 
    
    query '_(id, t) <-
      text(id; t).
    '
    
    query '_(id) <-
      onclick(id).
    '
    
    query '_(id) <-
      onkeydown(id).
    '
  """
  (removed, inserted, style, text, onclick, onkeydown) = lb(command)
  @js_(window, render($(col(removed, 1)), $(col(inserted,2)), $(col(inserted,3)), $(col(inserted,4)), $(col(inserted,5)), $(col(style,1)), $(col(style,2)), $(col(style,3)), $(col(text,1)), $(col(text,2)), $(col(onclick,1)), $(col(onkeydown,1))))
end

function render(window) 
  command = """
    open imp
    
    query '_(l, parent, ix, id, tag) <-
      node(id; parent, ix, tag),
      level(id; l).
    '   
    
    query '_(id, k, v) <-
      style(id, k; v).
    ' 
    
    query '_(id, t) <-
      text(id; t).
    ' 
    
    query '_(id) <-
      onclick(id).
    '
    
    query '_(id) <-
      onkeydown(id).
    '
  """
  (inserted, style, text, onclick, onkeydown) = @show lb(command)
  @js_(window, render($[], $(col(inserted,2)), $(col(inserted,3)), $(col(inserted,4)), $(col(inserted,5)), $(col(style,1)), $(col(style,2)), $(col(style,3)), $(col(text,1)), $(col(text,2)), $(col(onclick,1)), $(col(onkeydown,1))))
end

function window()
  window = Window()
  opentools(window)
  load!(window, "src/Imp.js")
  sleep(3) # :(
  handle(window, "event") do event
    @show event
    render(window, Symbol(event["table"]), event["values"])
  end
  innerHTML = "<div id=\"root\"></div>"
  @js(window, document.body.innerHTML = $innerHTML)
  render(window)
  window
end

begin 
  # dummy block for easy livecoding. we want upsertblock!
  command = """
    open imp
    
    addblock --name minesweeper '
      foo[] = d -> 
        int(d).
    '
  """
  lb(command)
end

begin 
  command = """
    open imp 
    
    removeblock minesweeper
    addblock --name minesweeper '
      state[] = s ->
        string(s).
        
      num_x[] = c ->
        int(c).
        
      num_y[] = c ->
        int(c).
      
      cell(id; x,y) ->
        string(id), int(x), int(y).
      
      mine(x,y) ->
        int(x), int(y).
      
      mine_count[x,y] = c ->
        int(x), int(y), int(c).
      
      cleared(x,y) ->
        int(x), int(y).
        
      num_cleared[] = c ->
        int(c).
        
      num_mines[] = c ->
        int(c).
        
      mine_x(i; x) -> 
        int(i), int(x).
        
      mine_x(i; x) <- 
        series<< x = rnd_uniform_int<1, 1000000000, 321>[i] >>
        int:range(1, num_mines[], 1, i).
        
      mine_y(i; y) -> 
        int(i), int(y).
          
      mine_y(i; y) <- 
        series<< y = rnd_uniform_int<1, 1000000000, 123>[i] >>
        int:range(1, num_mines[], 1, i).
        
      mine(x,y) <-
        int:range(1, num_mines[], 1, i),
        mine_x(i;xx),
        mine_y(i;yy),
        x = int:mod[xx, num_x[]],
        y = int:mod[yy, num_y[]].
      
      cell(id;x,y) <-
        int:range(1,num_x[],1,x),
        int:range(1,num_y[],1,y),
        id = "cell-" + int:string:convert[x] + "-" + int:string:convert[y].
        
      neighbouring_mine(x,y,nx,ny) <-
        cell(_;x,y),
        int:range(x-1,x+1,1,nx),
        int:range(y-1,y+1,1,ny),
        boolean:or(int:ne_3[nx,x],int:ne_3[ny,y], true),
        mine(nx,ny).
      
      mine_count[x,y] = c <-
        agg<< c=count() >>
        cell(_;x,y),
        neighbouring_mine(x,y,nx,ny).
        
      mine_count[x,y] = 0 <-
        cell(_;x,y),
        !neighbouring_mine(x,y,_,_).
        
      +cleared(x,y) <-
        +click(id),
        cell(id;x,y).
        
      +cleared(nx,ny) <-
        +cleared(x,y),
        mine_count(x,y;0),
        int:range(x-1,x+1,1,nx),
        int:range(y-1,y+1,1,ny),
        boolean:or(int:ne_3[nx,x],int:ne_3[ny,y], true),
        cell(_;nx,ny).
        
      num_cleared[] = c <-
        agg<< c=count() >>
        cleared(x,y).
        
      ^state[] = "game lost" <-
        +click(id),
        cell(id;x,y),
        mine(x,y).
        
      ^state[] = "game won" <-
        +click(id),
        cell(id;x,y),
        !mine(x,y),
        !state@prev[] = "game lost",
        num_cleared[] + num_mines[] = num_x[] * num_y[].
        
      node("grid"; "root", 1, "div"),
      style("grid", "display"; "flex"),
      style("grid", "flex-direction"; "column") <- .
      
      node(id; "grid", x, "div"),
      style(id, "display"; "flex"),
      style(id, "flex-direction"; "row") <-
        cell(_;x,_),
        id = "row-" + int:string:convert[x].
        
      node(id; parent, y, "button"),
      style(id, "width"; "2em"),
      style(id, "height"; "2em"),
      onclick(id) <-
        cell(id;x,y),
        parent = "row-" +  int:string:convert[x].
      
      text[id] = "_" <-
        cell(id;x,y),
        state[] = "game in progress",
        cleared(x,y),
        mine_count[x,y] = 0.
        
      text[id] = int:string:convert[c] <-
        cell(id;x,y),
        state[] = "game in progress",
        cleared(x,y),
        mine_count[x,y] = c,
        c != 0.
        
      text[id] = "M" <-
        cell(id;x,y),
        state[] = "game won",
        mine(x,y).
        
      text[id] = "*" <-
        cell(id;x,y),
        state[] = "game lost",
        mine(x,y).
        
      text[id] = int:string:convert[c] <-
        cell(id;x,y),
        state[] != "game in progress",
        !mine(x,y),
        mine_count[x,y] = c.
    '
    
    exec '
      ^state[] = "game in progress".
      
      ^num_x[] = 30.
      ^num_y[] = 30.
      ^num_mines[] = 90.
    '
    
    query '_(x,y,c) <- mine_count(x,y,c).'
  """
  lb(command)
end

# w = window()

end
