module Tests

import MacroTools
import MacroTools: @capture
using Imp
using Test

const globals = Dict{Symbol, Set}(
    :person => Set([("alice",), ("bob",), ("eve",)]),
    :string => Set([("alice",), ("bob",), ("eve",), ("cthulu",)]),
    :integer => Set([(0,), (1,), (2,)]),
    :evil => Set([("eve",), ("cthulu",)]),
    :rsvp => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
    :+ => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
    :points => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
)

function test_parse(expr)
    parsed = Imp.parse(expr)
    # cant test Imp.unparse(Imp.parse(expr)) == expr because of LineNumberNodes
    @test Imp.parse(Imp.unparse(parsed)) == parsed
end

macro test_parse(expr)
    :(test_parse($(QuoteNode(expr))))
end

@test_parse true
@test_parse (p -> 1)
@test_parse (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2)
@test_parse (x -> (y -> (person(x) & person(y)) | (x + y == 0)))

# --- analyze ---

function test_separate_scopes(expr)
    @test_throws Imp.CompileError imp(expr, passes=[:parse, :separate_scopes])
end

macro test_separate_scopes(expr)
    :(test_separate_scopes($(QuoteNode(expr))))
end

# catch scoping errors at compile time so we don't have to pay the cost of runtime env copying
@test_separate_scopes p -> q
@test_separate_scopes p -> (q -> true) & q

# --- interpret ---

function test_interpret(expr1, expr2)
    passes = [:parse, :separate_scopes, :interpret]
    @test imp(expr1, passes=passes, globals=globals) == imp(expr2, passes=passes, globals=globals)
end

macro test_interpret(expr)
    @assert @capture(expr, expr1_ == expr2_)
    :(test_interpret($(QuoteNode(expr1)), $(QuoteNode(expr2))))
end

@test Imp.set_to_bool(Imp.bool_to_set(true)) == true
@test Imp.set_to_bool(Imp.bool_to_set(false)) == false

# basic booleans
@test_interpret (true | true) == true
@test_interpret (true | false) == true
@test_interpret (false | true) == true
@test_interpret (false | false) == false
@test_interpret (true & true) == true
@test_interpret (true & false) == false
@test_interpret (false & true) == false
@test_interpret (false & false) == false
@test_interpret !true == false
@test_interpret !false == true

# 'implicit' boolean
@test_interpret person("alice") == true
@test_interpret person(0) == false
@test_interpret person == "alice" | "bob" | "eve"

# three-valued logic
@test_interpret rsvp("alice") == "yes"
@test_interpret rsvp("bob") == "no"
@test_interpret rsvp("eve") == nothing

# convert value to boolean
@test_interpret ("yes" == "yes") == true
@test_interpret ("yes" == "no") == false
@test_interpret (rsvp("alice") == "yes") == true
@test_interpret (rsvp("bob") == "yes") == false
@test_interpret (rsvp("eve") == "yes") == false
@test_interpret rsvp("alice", "yes") == true
@test_interpret rsvp("bob", "yes") == false
@test_interpret rsvp("eve", "yes") == false

# convert boolean to value
# (`if` is taken so `iff`)
@test_interpret if true "yes" else "no" end == "yes"
@test_interpret if false "yes" else "no" end == "no"
@test_interpret (true ? "yes" : "no") == "yes"
@test_interpret (false ? "yes" : "no") == "no"

# abstraction
@test_interpret (x -> true) == everything
@test_interpret (x -> false) == nothing
@test_interpret (p -> person(p)) == person
@test_interpret (x -> "alice")(2) == "alice"

# domain expressed via `iff`
@test_interpret (p -> if person(p) rsvp(p) end)("alice") == "yes"
@test_interpret (p -> if person(p) rsvp(p) end)("cthulu") == false
@test_interpret (p -> if person(p) rsvp(p, "yes") end) == "alice"
@test_interpret (p -> if person(p) rsvp(p, "no") end) == "bob"
@test_interpret (p -> rsvp(p, "no")) == ("bob" | "cthulu")

# defaults via `iff`
@test_interpret (p -> if person(p) rsvp(p) else "n/a" end)("alice") == "yes"
@test_interpret (p -> if person(p) rsvp(p) else "n/a" end)("cthulu") == "n/a"
@test_interpret (p -> if person(p) rsvp(p) else "n/a" end)(2) == "n/a"
@test_interpret (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") == "n/a"
@test_interpret (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2) == false

# can do quantification via eq

# exists == X :: not(eq(X, nothing))
@test_interpret exists(person) == true
@test_interpret exists(nothing) == false

# forall == X :: eq(X, everything)
@test_interpret forall(p -> person(p) => string(p)) == true
@test_interpret forall(p -> person(p) => evil(p)) == false
@test_interpret forall(p -> person(p) => rsvp(p, "yes")) == false
@test_interpret forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no"))) == false
@test_interpret forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no") | "eve"(p))) == true

# aggregation
@test_interpret reduce(+, 0, points) == 2

# tricky expressions
@test_interpret (x -> x(x)) == everything

# --- infer ---

function test_infer_types(expr, typs...)
    # TODO this is a big gross because of the global expr_types
    expr = imp(expr, passes=[:parse, :separate_scopes], globals=globals)
    imp(expr, passes=[:infer_types], globals=globals)
    inferred_type = Imp.expr_types[expr]
    @test inferred_type == Imp.SetType(typs)
end

macro test_infer_types(expr, typs...)
    :(test_infer_types($(QuoteNode(expr)), $(typs...)))
end

@test_infer_types true ()
@test_infer_types false
@test_infer_types 1 (Int64,)
@test_infer_types "yes" (String,)

@test_infer_types (true | true) ()
@test_infer_types (true & true) ()
@test_infer_types !true ()

@test_infer_types person("alice") ()
@test_infer_types person(0) 
@test_infer_types "alice" | "bob" | "eve" (String,)

@test_infer_types rsvp("alice") (String,)

@test_infer_types "yes" == "yes" ()
@test_infer_types rsvp("alice") == "yes" ()
@test_infer_types rsvp("alice", "yes") ()

@test_infer_types if true "yes" else "no" end (String,)
@test_infer_types if true "yes" else 0 end (Int64,) (String,)

@test_infer_types (x -> true) (String,) (Int64,)
@test_infer_types (p -> if person(p) rsvp(p) end) (String, String)
@test_infer_types (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end) (String, String)
@test_infer_types (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") (String,)

@test_infer_types exists(person) ()
@test_infer_types forall(p -> person(p) => string(p)) ()

@test_infer_types reduce(+, 0, points) (Int64,)

# harder cases

@test_infer_types (p -> p == 0) (Int64,)
@test_infer_types (p -> (p == 0) | string(p)) (Int64,) (String,)
@test_infer_types (p -> (p == 0) | (string(p) & integer(p))) (Int64,)
@test_infer_types (p -> if person(p) 0 else (if integer(p) "yes" end) end) (String, Int64) (Int64, String)
@test_infer_types (p -> if person(p) 0 else (if p == 0 "yes" end) end) (String, Int64) (Int64, String)

@test_infer_types (x -> (y -> (string(x) & string(y)) | (integer(x) & integer(y)))) (String, String) (Int64, Int64)
@test_infer_types (x -> (y -> (person(x) & person(y)) | (x + y == 0))) (String, String) (Int64, Int64)

# TODO need a way to represent true_type
# TODO need to know that string(x) is always true if x::String
# @test_infer_types (x -> if person(x); (if string(x) "yes" else 0 end) end) (String, String)

# TODO requires reasoning with sets as bounds rather than just types
# @test_infer_types (x -> if person(x); (if person(x) "yes" else 0 end) end) (String, String)

# --- lower ---

const lower_globals = Dict{Symbol, Set}([
   :f => Set([()]),
   :g => Set([(1,)]),
   :h => Set([(1,2)]),
])

function test_lower_apply(expr1, expr2)
    passes = [:parse, :separate_scopes, :interpret]
    lowered = imp(expr1, passes=[:parse, :separate_scopes, :infer_types, :lower_apply], globals=lower_globals)
    lower = imp(expr2, passes=[:parse, :separate_scopes], globals=lower_globals)
    @test lowered == lower
    # TODO lower should be idempotent, but has trouble with ==
    # relowered = imp(lowered, passes=[:infer_types, :lower_apply], globals=lower_globals)
    # @test lowered == relowered
end

macro test_lower_apply(expr)
    @assert @capture(expr, expr1_ => expr2_)
    :(test_lower_apply($(QuoteNode(expr1)), $(QuoteNode(expr2))))
end

# nothing to do
@test_lower_apply true => true
@test_lower_apply f() => f()
@test_lower_apply (x -> g(x)) => (_1 -> g(_1))
@test_lower_apply ((x,y) -> h(x,y)) => ((_1, _2) -> h(_1, _2))

@test_lower_apply (x -> h(x)) => ((_1, _2) -> h(_1, _2))
@test_lower_apply h => ((_1, _2) -> h(_1, _2))
@test_lower_apply (x -> x == g) => (_1 -> (_2 -> _2 == _1) == (_3 -> g(_3)))
@test_lower_apply h(g) => (_1 -> exists(_2 -> g(_2) & h(_2, _1)))
@test_lower_apply (x -> h(x) | g) => ((_1, _2) -> h(_1, _2) | g(_2))
@test_lower_apply g(0) => exists(_1 -> (_1 == 0) & g(_1))
@test_lower_apply !(g(0)) => !(exists(_1 -> (_1 == 0) & g(_1)))
@test_lower_apply (x -> y -> !(y(x))) => ((_1, _2) -> !(_1 == _2))

# --- bounded abstract ---

const bound_globals = Dict{Symbol, Set}([
    :x => Set(),
    :y => Set(),
    :z => Set(),
])

function test_simplify_bound(expr1, expr2)
    @test Imp.simplify_bound(imp(expr1, passes=[:parse], globals=bound_globals)) == imp(expr2, passes=[:parse], globals=bound_globals)
end

macro test_simplify_bound(expr1, expr2)
    :(test_simplify_bound($(QuoteNode(expr1)), $(QuoteNode(expr2))))
end

@test_simplify_bound nothing nothing
@test_simplify_bound everything everything
@test_simplify_bound (nothing & everything) nothing
@test_simplify_bound (nothing | everything) everything
@test_simplify_bound !(everything) nothing
@test_simplify_bound ((x & everything) | y) (x | y)
@test_simplify_bound ((x | nothing) & y) (x & y)
@test_simplify_bound ((x | everything) & y) y
@test_simplify_bound ((x & nothing) | y) y
@test_simplify_bound (x(y,z) | (y(x,z) & nothing)) x(y,z)
@test_simplify_bound x(nothing) x(nothing)

function test_bounded(expr)
    without_bounds = imp(expr, passes=[:parse, :separate_scopes, :infer_types, :interpret], globals=globals)
    # TODO use lower too
    with_bounds = imp(expr, passes=[:parse, :separate_scopes, :infer_types, :bound_abstract, :interpret], globals=globals, everything=Set([(0,),("a",)]))
    @test without_bounds == with_bounds
end

macro test_bounded(expr)
    :(test_bounded($(QuoteNode(expr))))
end

function test_unbounded(expr)
    without_bounds = imp(expr, passes=[:parse, :separate_scopes, :infer_types, :interpret], globals=globals)
    # TODO use lower too
    @test_throws KeyError imp(expr, passes=[:parse, :separate_scopes, :infer_types, :bound_abstract, :interpret], globals=globals, everything=nothing)
end

macro test_unbounded(expr)
    :(test_unbounded($(QuoteNode(expr))))
end

@test_bounded x -> false
@test_unbounded x -> true

@test_bounded p -> person(p)
@test_bounded (p, r) -> if person(p) rsvp(p, r) end
@test_bounded (x, y) -> person(x) & person(y)

end
