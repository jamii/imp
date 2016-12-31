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
  # run(`/home/jamie/logicblox/bin/lb web-server load-services`)
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
  command = """
    open imp 
    
    exec '
      ^minesweeper:minesweeper:state[] = "game in progress".
      
      ^minesweeper:minesweeper:num_x[] = 30.
      ^minesweeper:minesweeper:num_y[] = 30.
      ^minesweeper:minesweeper:num_mines[] = 90.
    '
  """
  lb(command)
end

# w = window()

# bin/lb compile project ../imp/imp.project &&elete imp && bin/lb create imp && bin/lb addproject imp ../imp && bin/lb web-server load-services

end
