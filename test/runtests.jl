module Tests

import MacroTools
import MacroTools: @capture
using Imp
using Test

env = merge(stdenv, Imp.Env{Set}(
    Imp.Var(:person) => Set([("alice",), ("bob",), ("eve",)]),
    Imp.Var(:string) => Set([("alice",), ("bob",), ("eve",), ("cthulu",)]),
    Imp.Var(:integer) => Set([(0,), (1,), (2,)]),
    Imp.Var(:evil) => Set([("eve",), ("cthulu",)]),
    Imp.Var(:rsvp) => Set([("alice", "yes"), ("bob", "no"), ("cthulu", "no")]),
    Imp.Var(:+) => Set([(a, b, (a+b) % 3) for a in 0:2 for b in 0:2]),
    Imp.Var(:points) => Set([("alice", 0), ("bob", 1), ("cthulu", 1)]),
))

macro test_parse(ast)
    quote
        expr = Imp.parse($(QuoteNode(ast)))
        # cant test Imp.unparse(Imp.parse(ast)) == ast because of LineNumberNodes
        @test Imp.parse(Imp.unparse(expr)) == expr
    end
end

@test_parse true
@test_parse (p -> 1)
@test_parse (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)(2)
@test_parse (x -> (y -> (person(x) & person(y)) | (x + y == 0)))

# --- analyze ---

# catch scoping errors at compile time so we don't have to pay the cost of runtime env copying
@test_throws Imp.CompileError @imp(env, p -> q)
@test_throws Imp.CompileError @imp(env, p -> (q -> true) & q)

# --- interpret ---

macro test_imp(ast)
    @assert @capture(ast, left_ == right_)
    # TODO not sure why these don't need escaping...
    :(@test @imp!($(copy(env)), $left) == @imp!($(copy(env)), $right))
end

@test Imp.set_to_bool(Imp.bool_to_set(true)) == true
@test Imp.set_to_bool(Imp.bool_to_set(false)) == false

# basic booleans
@test @imp!(env, true) == Set([()])
@test @imp!(env, false) == Set()
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

# --- infer ---

macro test_infer(expr, typs...)
    quote
        env = copy($env)
        expr = @imp(env, $expr)
        expr_types = Imp.infer(env, expr)
        @test expr_types[expr] == Imp.SetType([$(typs...)])
    end
end

macro test_infer_var(expr, vars_and_typs...)
    quote
        expr_types = Imp.infer(env, @imp(env, $expr))
        $(Imp.@splice var_and_typ in vars_and_typs begin
            @assert @capture(var_and_typ, var_ => typ_)
            :(@test expr_types[Imp.Var($(QuoteNode(var)), 1)] == Set($typ))
          end)
    end 
end

@test_infer true ()
@test_infer false
@test_infer 1 (Int64,)
@test_infer "yes" (String,)

@test_infer (true | true) ()
@test_infer (true & true) ()
@test_infer !true ()

@test_infer person("alice") ()
@test_infer person(0) 
@test_infer "alice" | "bob" | "eve" (String,)

@test_infer rsvp("alice") (String,)

@test_infer "yes" == "yes" ()
@test_infer rsvp("alice") == "yes" ()
@test_infer rsvp("alice", "yes") ()

@test_infer if true "yes" else "no" end (String,)
@test_infer if true "yes" else 0 end (Int64,) (String,)

@test_infer (x -> true) (String,) (Int64,)
@test_infer (p -> if person(p) rsvp(p) end) (String, String)
@test_infer (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end) (String, String)
@test_infer (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") (String,)

@test_infer exists(person) ()
@test_infer forall(p -> person(p) => string(p)) ()

@test_infer reduce(+, 0, points) (Int64,)

# harder cases

@test_infer (p -> p == 0) (Int64,)
@test_infer (p -> (p == 0) | string(p)) (Int64,) (String,)
@test_infer (p -> (p == 0) | (string(p) & integer(p))) (Int64,)
@test_infer (p -> if person(p) 0 else (if integer(p) "yes" end) end) (String, Int64) (Int64, String)
@test_infer (p -> if person(p) 0 else (if p == 0 "yes" end) end) (String, Int64) (Int64, String)

@test_infer (x -> (y -> (string(x) & string(y)) | (integer(x) & integer(y)))) (String, String) (Int64, Int64)
@test_infer (x -> (y -> (person(x) & person(y)) | (x + y == 0))) (String, String) (Int64, Int64)

# TODO need a way to represent true_type
# TODO need to know that string(x) is always true if x::String
# @test_infer (x -> if person(x); (if string(x) "yes" else 0 end) end) (String, String)

# TODO requires reasoning with sets as bounds rather than just types
# @test_infer (x -> if person(x); (if person(x) "yes" else 0 end) end) (String, String)

# --- lower ---

const lower_env = merge(stdenv, Imp.Env{Set}([Imp.Var(name) => Set() for name in [:f, :g, :h, :x, :y, :z]]))
const arities = Dict(Imp.Var(:f) => 0, Imp.Var(:g) => 1, Imp.Var(:h) => 2)

macro test_lower(higher, lower)
    :(@test Imp.lower($arities, @imp($(copy(lower_env)), $higher)) == @imp($(copy(lower_env)), $lower))
end

# nothing to do
# @test_lower true true
# @test_lower f() f()
# @test_lower x -> g(x) x -> g(x)
# @test_lower (x,y) -> h(x,y) (x,y) -> h(x,y)

# @test_lower x -> h(x) (x, __tmp__{1}) -> h(x, __tmp__{1})

# --- bounded abstract ---

const bounds_env = merge(stdenv, Imp.Env{Set}([Imp.Var(name) => Set() for name in [:x, :y, :z]]))
macro test_bound(original, simple)
    :(@test Imp.simplify_upper_bound(@imp($(copy(bounds_env)), $original)) == @imp($(copy(bounds_env)), $simple))
end

@test_bound nothing nothing
@test_bound everything everything
@test_bound (nothing & everything) nothing
@test_bound (nothing | everything) everything
@test_bound !(everything) nothing
@test_bound ((x & everything) | y) (x | y)
@test_bound ((x | nothing) & y) (x & y)
@test_bound ((x | everything) & y) y
@test_bound ((x & nothing) | y) y
@test_bound (x(y,z) | (y(x,z) & nothing)) x(y,z)
@test_bound x(nothing) x(nothing)

bounded_env = copy(env)
delete!(bounded_env, Imp.Var(:everything)) # no cheating

macro test_bounded(ast)
    :(@test @imp!($(copy(env)), $ast) == interpret($bounded_env, Imp.with_upper_bound(@imp($(copy(env)), $ast))))
end

macro test_unbounded(ast)
    :(@test_throws KeyError interpret($(copy(bounded_env)), Imp.with_upper_bound(@imp($(copy(bounded_env)), $ast))))
end

@test_bounded x -> false
@test_unbounded x -> true

@test_bounded p -> person(p)
@test_bounded (p, r) -> if person(p) rsvp(p, r) end
@test_bounded (x, y) -> person(x) & person(y)

end
