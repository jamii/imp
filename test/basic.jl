module Basic

import MacroTools
import MacroTools: @capture
using Imp
using Test

const globals = Dict{Symbol, Set}(
    :person => Set([("alice",), ("bob",), ("eve",)]),
    :string => Set([("alice",), ("bob",), ("eve",), ("cthulu",), ("n/a",)]),
    :integer => Set([(0,), (1,), (2,)]),
    :evil => Set([("eve",), ("cthulu",)]),
    :rsvp => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
    :+ => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
    :points => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
    :f => Set([()]),
    :g => Set([(1,)]),
    :h => Set([(1,2)]),
)

const everything = Set{Any}([(scalar,) for (_,set) in globals for row in set for scalar in row])

function test_imp_pass(env, expr, expected_inferred_type, expected_result, prev_inferred_type, prev_result)
    actual_inferred_type = Imp.infer_types(env, expr)[expr]
    # TODO this needs work
    # prev_inferred_type != nothing && @test prev_inferred_type == actual_inferred_type
    expected_inferred_type != nothing && @test issubset(actual_inferred_type, expected_inferred_type)

    actual_result = Imp.interpret(env, expr)
    prev_result != nothing && @test prev_result == actual_result
    expected_result != nothing && @test expected_result == actual_result

    actual_type = Imp.set_type(actual_result)
    @test issubset(actual_type, actual_inferred_type)

    (actual_inferred_type, actual_result)
end

function test_imp(raw_expr; lowered_expr=nothing, inferred_type=nothing, result=nothing, unboundable=false, everything=everything, globals=globals)
    name = string(raw_expr)
    name = replace(name, r"#=[^(=#)]*=#" => " ")
    name = replace(name, r"\n\s*"s => " ")
    @testset "$name" begin
        env = Imp.Env{Set}(Imp.Var(name) => set for (name, set) in globals)

        if everything != nothing
            env[Imp.Var(:everything)] = everything
        end
        expected_inferred_type = (inferred_type != nothing) ? Imp.SetType(inferred_type) : nothing
        expected_result = (result != nothing) ? imp(result, globals=globals, everything=everything, passes=[:parse, :interpret]) : nothing

        (prev_inferred_type, prev_result) = (nothing, nothing)

        expr = Imp.parse(raw_expr)
        # cant test Imp.unparse(Imp.parse(expr)) == expr because of LineNumberNodes
        @test Imp.parse(Imp.unparse(expr)) == expr

        expr = Imp.separate_scopes(Imp.Scope(env), expr)
        if everything != nothing
            (prev_inferred_type, prev_result) = test_imp_pass(env, expr, expected_inferred_type, expected_result, prev_inferred_type, prev_result)
        end

        expr = Imp.lower(env, expr)
        if lowered_expr != nothing
            @test expr == imp(lowered_expr, passes=[:parse], globals=globals)
        end
        if everything != nothing
            (prev_inferred_type, prev_result) = test_imp_pass(env, expr, expected_inferred_type, expected_result, prev_inferred_type, prev_result)
        end

        # TODO this needs work, especially around ==
        # relowered = Imp.lower(Imp.infer_types(env, expr), expr)
        # @test expr == relowered

        expr = Imp.bound_abstract(expr)
        (prev_inferred_type, prev_result) = test_imp_pass(env, expr, expected_inferred_type, expected_result, prev_inferred_type, prev_result)

        delete!(env, Imp.Var(:everything))
        if unboundable
            @test_throws KeyError Imp.interpret(env, expr)
        else
            (prev_inferred_type, prev_result) = test_imp_pass(env, expr, expected_inferred_type, expected_result, prev_inferred_type, prev_result)
        end
    end
end

imp(:( p -> person(p) => string(p) ), globals=globals, everything=everything)

# --- scoping ---

function test_separate_scopes(expr)
    @test_throws Imp.CompileError imp(expr, passes=[:parse], everything=everything)
end

test_separate_scopes(:(p -> q))
test_separate_scopes(:(p -> (q -> true) & q))

# --- semantics ---

@test Imp.set_to_bool(Imp.bool_to_set(true)) == true
@test Imp.set_to_bool(Imp.bool_to_set(false)) == false

# basic booleans
test_imp(:( true ))
test_imp(:( false ))
test_imp(:( (true | true) ), result=:( true ))
test_imp(:( (true | false) ), result=:( true ))
test_imp(:( (false | true) ), result=:( true ))
test_imp(:( (false | false) ), result=:( false ))
test_imp(:( (true & true) ), result=:( true ))
test_imp(:( (true & false) ), result=:( false ))
test_imp(:( (false & true) ), result=:( false ))
test_imp(:( (false & false) ), result=:( false ))
test_imp(:( !true ), result=:( false ))
test_imp(:( !false ), result=:( true ))

# 'implicit' boolean
test_imp(:( person("alice") ), result=:( true ))
test_imp(:( person(0) ), result=:( false ))
test_imp(:( person ), result=:( "alice" | "bob" | "eve" ))

# three-valued logic
test_imp(:( rsvp("alice") ), result=:("yes"))
test_imp(:( rsvp("bob") ), result=:("no"))
test_imp(:( rsvp("eve") ), result=:(nothing))

# convert value to boolean
test_imp(:( ("yes" == "yes") ), result=:( true ))
test_imp(:( ("yes" == "no") ), result=:( false ))
test_imp(:( (rsvp("alice") == "yes") ), result=:( true ))
test_imp(:( (rsvp("bob") == "yes") ), result=:( false ))
test_imp(:( (rsvp("eve") == "yes") ), result=:( false ))
test_imp(:( rsvp("alice", "yes") ), result=:( true ))
test_imp(:( rsvp("bob", "yes") ), result=:( false ))
test_imp(:( rsvp("eve", "yes") ), result=:( false ))


# convert boolean to value
test_imp(:( if true "yes" else "no" end ), result=:( "yes" ))
test_imp(:( if false "yes" else "no" end ), result=:( "no" ))
test_imp(:( (true ? "yes" : "no") ), result=:( "yes" ))
test_imp(:( (false ? "yes" : "no") ), result=:( "no" ))

# abstraction
test_imp(:( (x -> true) ), result=:( everything ), unboundable=true)
test_imp(:( (x -> false) ), result=:( nothing ))
test_imp(:( (p -> person(p)) ), result=:( person ))
test_imp(:( (x -> "alice")(2) ), result=:( "alice" ))

# domain expressed via `if`
test_imp(:( (p -> if person(p) rsvp(p) end)("alice") ), result=:( "yes" ))
test_imp(:( (p -> if person(p) rsvp(p) end)("cthulu") ), result=:( false ))
test_imp(:( (p -> if person(p) rsvp(p, "yes") end) ), result=:( "alice" ))
test_imp(:( (p -> if person(p) rsvp(p, "no") end) ), result=:( "bob" ))
test_imp(:( (p -> rsvp(p, "no")) ), result=:( ("bob" | "cthulu") ))

# defaults via `if`
test_imp(:( (p -> if person(p) rsvp(p) else "n/a" end)("alice") ), result=:( "yes" ))
test_imp(:( (p -> if person(p) rsvp(p) else "n/a" end)("cthulu") ), result=:( "n/a" ))
test_imp(:( (p -> if person(p) rsvp(p) else "n/a" end)(2) ), result=:( "n/a" ))
test_imp(:( (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") ), result=:( "n/a" ))
test_imp(:( (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2) ), result=:( false ))

# can do quantification via eq

# exists == X :: not(eq(X, nothing))
test_imp(:( exists(person) ), result=:( true ))
test_imp(:( exists(nothing) ), result=:( false ))

# forall == X :: eq(X, everything)
test_imp(:( forall(p -> person(p) => string(p)) ), result=:( true ))
test_imp(:( forall(p -> person(p) => evil(p)) ), result=:( false ))
test_imp(:( forall(p -> person(p) => rsvp(p, "yes")) ), result=:( false ))
test_imp(:( forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no"))) ), result=:( false ))
test_imp(:( forall(p -> person(p) => (rsvp(p, "yes") | rsvp(p, "no") | "eve"(p))) ), result=:( true ))

# aggregation
test_imp(:( reduce(+, 0, points) ), result=:( 2 ))

# tricky expressions
test_imp(:( (x -> x(x)) ), result=:( everything ), unboundable=true)

# let
test_imp(:( let x = 1; x + 1 end ), result=:( 2 ))
test_imp(:( let inc = x -> x + 1; inc(1) end ), result=:( 2 ))
test_imp(:( let inc = x -> x + 1; inc(inc(1)) end ), result=:( 0 ))

# tuples
test_imp(:( (1,2) ), result=:( (a,b) -> (a==1) & (b==2) ))
test_imp(:( (person, rsvp) ), result=:( (a,b,c) -> person(a) & rsvp(b,c) ))
test_imp(:( (rsvp, false) ), result=:( false ))
test_imp(:( ("alice", "yes")|("bob", "no")|("cthulu", "no") ), result=:( rsvp ))

# wildcards
test_imp(:( _ ), result=:( x -> true ), unboundable=true)
test_imp(:( r -> rsvp(_, r) ), result=:( r -> exists(p -> rsvp(p, r)) ))
test_imp(:( rsvp(_) ), result=:( r -> exists(p -> rsvp(p, r)) ))

test_imp(:( p -> if person(p); (r -> true) end ), unboundable=true)

# --- infer_types ---

test_imp(:( true ), inferred_type=[()])
test_imp(:( false ), inferred_type=[])
test_imp(:( 1 ), inferred_type=[(Int64,)])
test_imp(:( "yes" ), inferred_type=[(String,)])

test_imp(:( (true | true) ), inferred_type=[()])
test_imp(:( (true & true) ), inferred_type=[()])
test_imp(:( !true ), inferred_type=[()])

test_imp(:( person("alice") ), inferred_type=[()])
test_imp(:( person(0) ), inferred_type=[])
test_imp(:( "alice" | "bob" | "eve" ), inferred_type=[(String,)])

test_imp(:( rsvp("alice") ), inferred_type=[(String,)])

test_imp(:( "yes" == "yes" ), inferred_type=[()])
test_imp(:( rsvp("alice") == "yes" ), inferred_type=[()])
test_imp(:( rsvp("alice", "yes") ), inferred_type=[()])

test_imp(:( if true "yes" else "no" end ), inferred_type=[(String,)])
test_imp(:( if true "yes" else 0 end ), inferred_type=[(Int64,), (String,)])

test_imp(:( (x -> true) ), inferred_type=[(String,), (Int64,)], unboundable=true)
test_imp(:( (p -> if person(p) rsvp(p) end) ), inferred_type=[(String, String)])
test_imp(:( (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end) ), inferred_type=[(String, String)])
test_imp(:( (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") ), inferred_type=[(String,)])

test_imp(:( exists(person) ), inferred_type=[()])
test_imp(:( forall(p -> person(p) => string(p)) ), inferred_type=[()])

test_imp(:( reduce(+, 0, points) ), inferred_type=[(Int64,)])

test_imp(:( p -> rsvp(rsvp(rsvp(p))) ), inferred_type=[(String,String)])

# harder cases

test_imp(:( (p -> p == 0) ), inferred_type=[(Int64,)])
test_imp(:( (p -> (p == 0) | string(p)) ), inferred_type=[(Int64,), (String,)])
test_imp(:( (p -> (p == 0) | (string(p) & integer(p))) ), inferred_type=[(Int64,)])
test_imp(:( (p -> if person(p) 0 else (if integer(p) "yes" end) end) ), inferred_type=[(String, Int64), (Int64, String)])
test_imp(:( (p -> if person(p) 0 else (if p == 0 "yes" end) end) ), inferred_type=[(String, Int64), (Int64, String)])

test_imp(:( (x -> (y -> (string(x) & string(y)) | (integer(x) & integer(y)))) ), inferred_type=[(String, String), (Int64, Int64)])
# TODO x+y==0 is tricky to lower correctly
# test_imp(:( (x -> (y -> (person(x) & person(y)) | (x + y == 0))) ), inferred_type=[(String, String), (Int64, Int64)])

# TODO need a way to represent true_type
# TODO need to know that string(x) is always true if x::String
# test_imp(:( (x -> if person(x); (if string(x) "yes" else 0 end) end) ), inferred_type=[(String, String)])

# TODO requires reasoning with sets as bounds rather than just types
# test_imp(:( (x -> if person(x); (if person(x) "yes" else 0 end) end) ), inferred_type=[(String, String)])

# --- lower ---

# nothing to do
test_imp(:( true ), lowered_expr=:( () -> true ))
test_imp(:( f() ), lowered_expr=:( () -> f() ))
test_imp(:( (x -> g(x)) ), lowered_expr=:( (_1 -> g(_1)) ))
test_imp(:( (x,y) -> h(x,y) ), lowered_expr=:( (_1, _2) -> h(_1, _2) ))

# actual lowering
test_imp(:( (x -> h(x)) ), lowered_expr=:( (_1, _2) -> h(_1, _2) ))
test_imp(:( h ), lowered_expr=:( (_1, _2) -> h(_1, _2) ))
# TODO x==g is tricky to lower correctly
# test_imp(:( (x -> x == g) ), lowered_expr=:( _1 -> (_2 -> _2 == _1) == (_3 -> g(_3)) ))
test_imp(:( h(g) ), lowered_expr=:( _1 -> exists(_2 -> g(_2) & h(_2, _1)) ))
# TODO raise_union creates duplicate vars - does that need to be fixed?
# test_imp(:( (x -> h(x) | g) ), lowered_expr=:( ((_1, _2) -> h(_1, _2)) | ((_1, _2) -> g(_2)) ), unboundable=true)
test_imp(:( g(0) ), lowered_expr=:( () -> exists(_1 -> (_1 == 0) & g(_1)) ))
test_imp(:( !(g(0)) ), lowered_expr=:( () -> (() -> !(_1 -> (_1 == 0) & g(_1))) ))
test_imp(:( (x -> y -> !(y(x))) ), lowered_expr=:( (_1, _2) -> (() -> !(_1 == _2)) ), unboundable=true)

# --- simplify_bound ---

const bound_globals = Dict{Symbol, Set}([
    :x => Set(),
    :y => Set(),
    :z => Set(),
])

function test_simplify_bound(expr1, expr2)
    @test Imp.simplify_bound(Imp.parse(expr1)) == Imp.parse(expr2)
end

macro test_simplify_bound(expr1, expr2)
    :(test_simplify_bound($(QuoteNode(expr1)), $(QuoteNode(expr2))))
end

@test_simplify_bound nothing nothing
@test_simplify_bound everything everything
@test_simplify_bound (nothing & everything) nothing
@test_simplify_bound (nothing | everything) everything
@test_simplify_bound ((x & everything) | y) (x | y)
@test_simplify_bound ((x | nothing) & y) (x & y)
@test_simplify_bound ((x | everything) & y) y
@test_simplify_bound ((x & nothing) | y) y
@test_simplify_bound (x(y,z) | (y(x,z) & nothing)) x(y,z)
@test_simplify_bound x(nothing) x(nothing)

end
