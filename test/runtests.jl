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

macro test_imp(ast)
    @assert @capture(ast, left_ == right_)
    # TODO not sure why these don't need escaping...
    :(@test @imp($env, $left) == @imp($env, $right))
end

@test Imp.set_to_bool(Imp.bool_to_set(true)) == true
@test Imp.set_to_bool(Imp.bool_to_set(false)) == false

# basic booleans
@test @imp(env, true) == Set([()])
@test @imp(env, false) == Set()
@test_imp (true | true) == true
@test_imp (true | false) == true
@test_imp (false | true) == true
@test_imp (false | false) == false
@test_imp (true & true) == true
@test_imp (true & false) == false
@test_imp (false & true) == false
@test_imp (false & false) == false
@test_imp !true == false
@test_imp !false == true

# 'implicit' boolean
@test_imp person("alice") == true
@test_imp person(0) == false
@test_imp person == "alice" | "bob" | "eve"

# three-valued logic
@test_imp rsvp("alice") == "yes"
@test_imp rsvp("bob") == "no"
@test_imp rsvp("eve") == nothing

# convert value to boolean
@test_imp ("yes" == "yes") == true
@test_imp ("yes" == "no") == false
@test_imp (rsvp("alice") == "yes") == true
@test_imp (rsvp("bob") == "yes") == false
@test_imp (rsvp("eve") == "yes") == false
@test_imp rsvp("alice", "yes") == true
@test_imp rsvp("bob", "yes") == false
@test_imp rsvp("eve", "yes") == false

# convert boolean to value
# (`if` is taken so `iff`)
@test_imp if true "yes" else "no" end == "yes"
@test_imp if false "yes" else "no" end == "no"
@test_imp (true ? "yes" : "no") == "yes"
@test_imp (false ? "yes" : "no") == "no"

# abstraction
@test_imp (x -> true) == everything
@test_imp (x -> false) == nothing
@test_imp (p -> person(p)) == person
@test_imp (x -> "alice")(2) == "alice"

# domain expressed via `iff`
@test_imp (p -> if person(p) rsvp(p) end)("alice") == "yes"
@test_imp (p -> if person(p) rsvp(p) end)("cthulu") == false
@test_imp (p -> if person(p) rsvp(p, "yes") end) == "alice"
@test_imp (p -> if person(p) rsvp(p, "no") end) == "bob"
@test_imp (p -> rsvp(p, "no")) == ("bob" | "cthulu")

# defaults via `iff`
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)("alice") == "yes"
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)("cthulu") == "n/a"
@test_imp (p -> if person(p) rsvp(p) else "n/a" end)(2) == "n/a"
@test_imp (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") == "n/a"
@test_imp (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2) == false

# can do quantification via eq

# exists == X :: not(eq(X, nothing))
@test_imp exists(person) == true
@test_imp exists(nothing) == false

# forall == X :: eq(X, everything)
@test_imp forall(p -> person(p) => string(p)) == true
@test_imp forall(p -> person(p) => evil(p)) == false
@test_imp forall(p -> person(p) => rsvp(p, "yes")) == false
@test_imp forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no"))) == false
@test_imp forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no") | "eve"(p))) == true

# aggregation
@test_imp reduce(+, 0, points) == 2

# tricky expressions
@test_imp (x -> x(x)) == everything

end
