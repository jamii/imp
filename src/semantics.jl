struct Apply1 <: Expr
    f::Expr
    arg::Expr
end

struct Abstract1 <: Expr
    var::Symbol
    value::Expr
end

function unparse(expr::Union{Apply1, Abstract1})
    @match expr begin
        Apply1(f, arg) => :($(unparse(f))($(unparse(arg))))
        Abstract1(var, arg) => :($(unparse(var)) -> $(unparse(arg)))
    end
end

function desugar(expr)
    @match expr begin
        _::Constant => expr
        _::Var => expr
        Apply(f, args) => reduce(Apply1, desugar(f), map(desugar, args))
        Primitive(f, args) => Primitive(f, map(desugar, args))
        Abstract(vars, value) => reduce((value, var) -> Abstract1(var, value), desugar(value), reverse(vars))
    end
end

const Env = Dict{Symbol, Set}

const truthy = Set([()])
const falsey = Set()
bool_to_set(bool::Bool)::Set = bool ? truthy : falsey
set_to_bool(set::Set)::Bool = length(set) > 0

function interpret(env::Env, expr::Constant) ::Set
    Set([(expr.value,)])
end

function interpret(env::Env, expr::Constant{Bool}) ::Set
    bool_to_set(expr.value)
end

function interpret(env::Env, expr::Var) ::Set
    env[expr.name]
end

function interpret(env::Env, expr::Apply1) ::Set
    f = interpret(env, expr.f)
    arg = interpret(env, expr.arg)
    result = Set()
    for n in map(length, arg)
        for row in f
            if (length(row) >= n) && (row[1:n] in arg)
                push!(result, row[n+1:end])
            end
        end
    end
    result
end

function interpret(env::Env, expr::Abstract1) ::Set
    env = copy(env)
    result = Set()
    for domain_row in env[:everything]
        env[expr.var] = Set([domain_row])
        value = interpret(env, expr.value)
        for value_row in value
            push!(result, (domain_row..., value_row...))
        end
    end
    result
end

function interpret(env::Env, expr::Primitive) ::Set
    args = [interpret(env, arg) for arg in expr.args]
    @match (expr.f, args) begin
        (:|, _) => union(args...)
        (:&, _) => intersect(args...)
        (:!, [arg]) => bool_to_set(!set_to_bool(arg))
        (:(=>), [a, b]) => bool_to_set((!set_to_bool(a) || set_to_bool(b)))
        (:(==), _) => bool_to_set((==)(args...))
        (:iff, [cond, true_branch, false_branch]) => set_to_bool(cond) ? true_branch : false_branch
        (:reduce, [raw_op, raw_init, values]) => begin
            op = Dict(((a,b) => c for (a,b,c) in raw_op))
            @assert length(raw_init) == 1
            @assert length(first(raw_init)) == 1
            init = first(raw_init)[1]
            value = reduce((a,b) -> op[a,b[end]], init, values)
            Set([(value,)])
        end
        (:exists, [arg]) => bool_to_set(arg != env[:nothing])
        (:forall, [arg]) => bool_to_set(arg == env[:everything])
        _ => error("Unknown primitive: $expr")
    end
end

stdenv = Dict{Symbol, Set}(
  :everything => Set{Any}([(scalar,) for scalar in [0, 1, 2, "alice", "bob", "eve", "cthulu", "yes", "no"]]),
  :nothing => Set([]),
  )
