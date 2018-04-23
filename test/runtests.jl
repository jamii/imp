module Tests

import MacroTools
import MacroTools: @capture
using Imp
using Test

env = merge(stdenv, Dict{Symbol, Set}(
:person => Set([("alice",), ("bob",), ("eve",)]),
:string => Set([("alice",), ("bob",), ("eve",), ("cthulu",)]),
:evil => Set([("eve",), ("cthulu",)]),
:rsvp => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
:+ => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
:points => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
))

include("test_semantics.jl")
include("test_inference.jl")

end
