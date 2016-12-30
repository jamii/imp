module LB

using Blink

function lb(command)
  write("/tmp/script.lb", command)
  output = readstring(`/home/jamie/logicblox-x86_64-linux-4.3.17-a3f4888aecd21c326197b02deae87d1d74569cc5/bin/lb /tmp/script.lb`)
  raw_results = matchall(r"/--------------- _ ---------------\\([^$\\]*)\\--------------- _ ---------------/"s, output)
  @show raw_results
  results = [readdlm(IOBuffer(raw_result[37:(length(raw_result)-36)])) for raw_result in raw_results]
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
      
    click(id) ->
      string(id).
    lang:pulse(`click).
      
    onkeydown(id) ->
      string(id).
      
    keydown(id; key, text) ->
      string(id), int(key), string(text).
    lang:pulse(`keydown).
      
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
    '
    
    query '_(l, id) <-
      node(id; _, _, _),
      level(id; l).
    '    
  """
  lb(command)
end

function render(window, event_string) 
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
  """
  (removed, inserted) = lb(command)
  removed_id = removed[:,1]
  parent = inserted[:,2]
  ix = inserted[:,3]
  id = inserted[:,4]
  tag = inserted[:,5]
  @js(window, render($removed_id, $parent, $ix, $id, $tag, $[], $[], $[], $[], $[], $[]))
end

function render(window) 
  command = """
    open imp
    
    query '_(l, parent, ix, id, tag) <-
      node(id; parent, ix, tag),
      level(id; l).
    '    
  """
  (inserted,) = lb(command)
  parent = inserted[:,2]
  ix = inserted[:,3]
  id = inserted[:,4]
  tag = inserted[:,5]
  @js(window, render($[], $parent, $ix, $id, $tag, $[], $[], $[], $[], $[], $[]))
end

function window()
  window = Window()
  opentools(window)
  load!(window, "src/Imp.js")
  sleep(3) # :(
  handle(window, "event") do event
    @show event
    render(window, "")
  end
  innerHTML = "<div id=\"root\"></div>"
  @js(window, document.body.innerHTML = $innerHTML)
  render(window)
  window
end

window()

end
