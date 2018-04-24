env_types = Dict{Symbol, Imp.SetType}((name => Imp.SetType([map(typeof, row) for row in set]) for (name, set) in env))

macro test_infer(expr, typs...)
    :(@test infer($env_types, @imp($expr)) == Imp.SetType([$(typs...)]))
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

@test_infer (x -> true) (String,) (Int64,) (Float64,)
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
