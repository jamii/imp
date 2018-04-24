module Tests

import MacroTools
import MacroTools: @capture
using Imp
using Test

env = merge(stdenv, Dict{Symbol, Set}(
    :person => Set([("alice",), ("bob",), ("eve",)]),
    :string => Set([("alice",), ("bob",), ("eve",), ("cthulu",)]),
    :integer => Set([(0,), (1,), (2,)]),
    :evil => Set([("eve",), ("cthulu",)]),
    :rsvp => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
    :+ => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
    :points => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
))

@testset "semantics" begin
    include("test_semantics.jl")
end
@testset "inference" begin 
    include("test_inference.jl")
end

end
