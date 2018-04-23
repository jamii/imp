env_types = Dict{Symbol, Imp.RowType}((name => isempty(set) ? () : map(typeof, first(set)) for (name, set) in env))

macro test_infer(expr, typ)
    :(@test infer($env_types, @imp($expr)) == $typ)
end

@test_infer true ()
@test_infer false ()
@test_infer 1 (Int64,)
@test_infer "yes" (String,)

@test_infer (true | true) ()
@test_infer (true & true) ()
@test_infer !true ()

@test_infer person("alice") ()
@test_infer person(0) ()
@test_infer "alice" | "bob" | "eve" (String,)

@test_infer rsvp("alice") (String,)

@test_infer "yes" == "yes" ()
@test_infer rsvp("alice") == "yes" ()
@test_infer rsvp("alice", "yes") ()

@test_infer if true "yes" else "no" end (String,)
@test_infer if true "yes" else 0 end (Union{Int64, String},)

@test_infer (x -> true) (Any,)
# @test_infer (p -> if person(p) rsvp(p) end) (String,)
# @test_infer (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end) (String, String)
# @test_infer (p -> if person(p) rsvp(p) else (if string(p) "n/a" end) end)("cthulu") (String,)

@test_infer exists(person) ()
@test_infer forall(p -> person(p) => string(p)) ()

@test_infer reduce(+, 0, points) (Int64,)
