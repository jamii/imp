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

function parse(code, cache)
  codelets = split(code, "\n\n")
  values = []
  for codelet in codelets
    value = try
      get(cache, codelet) do
        eval(Base.parse(codelet))
      end
    catch error
      error
    end
    push!(values, value)
  end
  flow = Sequence([value for value in values if value isa Flow])
  uis = [value for value in values if value isa Expr]
  others = [value for value in values if !(value isa Flow) && !(value isa Expr)] 
  (flow, uis, others)
end

function run(view, code, cache)
  (flow, uis, others) = parse(code, cache)
  set_flow!(view, flow)
  @show others
  if !isempty(uis)
    set_template!(view, uis[1])
  end
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
      @show :processing event
      if haskey(event, "code")
        @showtime run(view, event["code"], cache)
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
