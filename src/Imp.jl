module Imp

import MacroTools
import MacroTools: @capture
using Rematch

# TODO remove workaround for https://github.com/JuliaLang/julia/issues/26885
function Base.show(io::IO, set::Set)
    print(io, "Set(", collect(set), ")")
end

include("util.jl")
include("semantics.jl")
include("inference.jl")

macro imp(ast)
    parse(ast)
end

macro imp(env, ast)
    :(interpret($(esc(env)), parse($(QuoteNode(ast)))))
end

export stdenv, @imp, interpret, infer

end
