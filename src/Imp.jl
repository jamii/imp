module Imp

import MacroTools
import MacroTools: @capture
using Rematch

# TODO remove workaround for https://github.com/JuliaLang/julia/issues/26885
function Base.show(io::IO, set::Set)
    print(io, "Set(", collect(set), ")")
end

include("util.jl")
include("syntax.jl")
include("semantics.jl")
include("inference.jl")
include("compile.jl")

macro imp(ast)
    desugar(parse(ast))
end

macro imp(env, ast)
    # TODO not sure why ast doesn't need escaping...
    :(interpret($(esc(env)), @imp($ast)))
end

export stdenv, @imp, interpret, infer

end
