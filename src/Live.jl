module Live

using Util
using Data
using Query
using Flows
using UI
using Match

using HttpServer
using WebSockets
using JSON

function parse(code, cursor, cache)
  codelets = split(code, "\n\n")
  values = []
  @showtime for codelet in codelets
    value = try
      get(cache, codelet) do
        eval(Base.parse(codelet))
      end
    catch error
      error
    end
    push!(values, value)
  end
  focused = nothing
  for i in 1:length(codelets)
    if cursor < length(codelets[i])
      focused = values[i]
      break
    else
      cursor -= length(codelets[i])
      cursor -= 2 # \n\n
    end
  end
  flow = Sequence([value for value in values if value isa Flow])
  others = [value for value in values if !(value isa Flow) && !(value isa Expr)] 
  (flow, focused, others)
end

const empty_template = Template(quote [html [head] [body]] end, View())

function run(view, code, cursor, cache)
  (flow, focused, others) = parse(code, cursor, cache)
  @showtime view.world.flow = Sequence([
    @stateful session(Session)
    flow
  ])
  @show others
  if focused isa Expr
    @showtime view.template = Template(focused, view)
  else
    @showtime view.template = empty_template
  end
  @showtime refresh(view)
end

function watch(view, filename, cache)
  open(filename) do file
    run(view, readstring(file), cache)
  end
  while true
    watch_file(filename)
    @show :running
    open(filename) do file
      run(view, readstring(file), cache)
    end
  end
end

function serve(view, cache)
  channel = Channel(32)
  handler = WebSocketHandler() do req,client
    begin
      while true
        bytes = try
          read(client)
        catch error
          info(error)
          return
        end
        put!(channel, bytes)
      end
    end
  end
  server = Server(handler)
  server_task = @async HttpServer.run(server,8081)
  runner_task = @async begin
    while true
      bytes = take!(channel)
      while isready(channel)
        bytes = take!(channel)
      end
      event = JSON.parse(String(bytes))
      @show :processing
      if haskey(event, "code") && haskey(event, "cursor")
        @showtime run(view, event["code"], event["cursor"], cache)
      end
    end
  end
  bind(channel, server_task)
  bind(channel, runner_task)
  server
end


function todo()
  view = View()
  cache = Dict{String, Any}()
  ui_server = @async UI.serve(view)
  code_server = @async serve(view, cache)
  (ui_server, code_server)
end

end
