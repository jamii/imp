module LB

using Blink
using DataFrames

function lb(command)
  write("/tmp/script.lb", command)
  output = readstring(`/home/jamie/logicblox-x86_64-linux-4.3.17-a3f4888aecd21c326197b02deae87d1d74569cc5/bin/lb /tmp/script.lb`)
  raw_results = matchall(r"/--------------- _ ---------------\\([^$\\]*)\\--------------- _ ---------------/"s, output)
  stripped_results = [raw_result[37:(length(raw_result)-36)] for raw_result in raw_results]
  @show stripped_results
  results = Any[readtable(IOBuffer(String(stripped_result)), separator=' ', header=false) for stripped_result in stripped_results]
  @show results
end

begin
  init = """
  open imp
  
  addblock '
    node(id; parent, ix, tag) ->
      string(id), string(parent), int(ix), string(tag).
      
    style(id, key; val) ->
      string(id), string(key), string(val).
      
    text(id; val) ->
      string(id), string(val).
      
    onclick(id) ->
      string(id).
      
    lang:pulse(`click).
    click(id) ->
      string(id).
      
    onkeydown(id) ->
      string(id).
      
    lang:pulse(`keydown).
    keydown(id; key, text) ->
      string(id), int(key), string(text).
      
    level(id; val) ->
      string(id), int(val).
      
    level("root"; 1).
    
    level(id; val+1) <-
      level(parent; val),
      node(id; parent, _, _).
  '

  """
  lb(init)
end

begin 
  command = """
    open imp
    
    exec '
      -node(id; _, _, _) <-
        node@prev(id; _, _, _).
    '
  """
  lb(command)
end

begin 
  command = """
    open imp
    
    exec '
      +node("a"; "root", 1, "div").
      +node("b"; "root", 2, "div").
      +node("c"; "a", 1, "button").
      
      +text("c"; "click me!").
      
      +onclick("c").
    '
    
    query '_(l, id) <-
      node(id; _, _, _),
      level(id; l).
    '    
  """
  lb(command)
end

function col(dataframe, i)
  (size(dataframe) == (0,0)) ? Any[] : dataframe[i].data
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
  @js(window, render($(col(removed, 1)), $(col(inserted,2)), $(col(inserted,3)), $(col(inserted,4)), $(col(inserted,5)), $(col(style,1)), $(col(style,2)), $(col(style,3)), $(col(text,1)), $(col(text,2)), $(col(onclick,1)), $(col(onkeydown,1))))
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
  @js(window, render($[], $(col(inserted,2)), $(col(inserted,3)), $(col(inserted,4)), $(col(inserted,5)), $(col(style,1)), $(col(style,2)), $(col(style,3)), $(col(text,1)), $(col(text,2)), $(col(onclick,1)), $(col(onkeydown,1))))
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

w = window()

end
