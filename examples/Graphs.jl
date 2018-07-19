module Graphs

using ..Imp
using CSV
using DataFrames
using Test

function cache(f, key)
    try
        task_local_storage(key)
    catch
        task_local_storage(key, f())
    end
end

function load_pokec()
    df = CSV.read("../soc-pokec-relationships.txt", delim='\t', header=["a","b"])
    cs = DataFrames.columns(df)
    as = Int32[]
    bs = Int32[]
    for i in 1:length(cs[1])
        a = cs[1][i]
        b = cs[2][i]
        if a != b
            push!(as, min(a,b))
            push!(bs, max(a,b))
        end
    end
    (as,bs)
end

function index((as, bs))
    x = Imp.Finger((as , bs))
    y = typeof(x)(x.columns, x.range, x.default)
    z = typeof(x)(x.columns, x.range, x.default)
    Imp.quicksort!(x.columns)
    (x,y,z)
end

struct LT{ixes, Tail}
    tail::Tail
end
LT(ixes, tail) = LT{ixes, typeof(tail)}(tail)

Imp.start(lt::LT, fingers) = Imp.start(lt.tail, fingers)
Imp.finish(lt::LT, state) = Imp.finish(lt.tail, state)
function Imp.execute(lt::LT{ixes}, in_fingers, state) where ixes
    tail = lt.tail
    ((a,ca),(b,cb)) = ixes
    if isless(Imp.finger_get(in_fingers[a], Val{ca}), Imp.finger_get(in_fingers[b], Val{cb}))
        Imp.execute(tail, in_fingers, state)
    end
end

function join((x,y,z))
    query =
        Imp.GenericJoin((1,3),
                        Imp.GenericJoin((1,2),
                                        Imp.GenericJoin((2,3),
                                                        Imp.Count())))
    # Imp.Select(((1,1),(1,2),(2,2))))))
    Imp.run(query, (x,y,z))
end

macro show_benchmark(b)
    quote
        println($(string(b)))
        # TODO don't understand why nested macros no longer require esc
        display(@benchmark($b))
    end
end

function bench()
    data = @time load_pokec()
    fingers = @time index(data)
    result = @time join(fingers)
    result = @time join(fingers)
    @test result == 32557458
end

bench()

end
