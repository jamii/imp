module Live

using Data
using Query
using Flows
using UI
using Match

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

function todo()
    view = View()
    cache = Dict{String, Any}()
    @async UI.serve(view)
    watch(view, "examples/LiveTodo.jl", cache)
end

end
