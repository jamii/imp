abstract type Expr end

struct Constant{T} <: Expr
    value::T # scalar
end

struct Var <: Expr
    name::Symbol
end

struct Apply <: Expr
    f::Expr
    args::Vector{Expr}
end

struct Abstract <: Expr
    variable::Symbol
    value::Expr
end

# things which operate on relations rather than on values
struct Primitive <: Expr
    f::Symbol
    args::Vector{Expr}
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

function interpret(env::Env, expr::Apply) ::Set
    f = interpret(env, expr.f)
    for arg in map((arg) -> interpret(env, arg), expr.args)
        result = Set()
        if !isempty(arg)
            n = length(first((arg)))
            for row in f
                if row[1:n] in arg
                    push!(result, row[n+1:end])
                end
            end
            f = result
        end
    end
    f
end

function interpret(env::Env, expr::Abstract) ::Set
    env = copy(env)
    result = Set()
    for domain_row in env[:everything]
        env[expr.variable] = Set([domain_row])
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

function parse(ast)
    if @capture(ast, constant_Int64_String_Bool)
        Constant(constant)
    elseif @capture(ast, name_Symbol)
        Var(name)
    elseif @capture(ast, f_(args__)) && length(args) > 0
        if f in [:|, :&, :!, :(=>), :(==), :reduce, :exists, :forall]
            Primitive(f, map(parse, args))
        else
            Apply(parse(f), map(parse, args))
        end
    elseif @capture(ast, if cond_ true_branch_ end)
        Primitive(:iff, [parse(cond), parse(true_branch), Constant(false)])
    elseif @capture(ast, if cond_ true_branch_ else false_branch_ end)
        Primitive(:iff, [parse(cond), parse(true_branch), parse(false_branch)])
    elseif @capture(ast, (variable_Symbol) -> value_)
        Abstract(variable, parse(value))
    else
        error("Unknown syntax: $ast")
    end
end

function unparse(expr)
    @match expr begin
        Constant(value) => value
        Var(name) => name
        Apply(f, args) => :($(unparse(f))($(map(unparse, args)...)))
        Abstract(variable, value) => :(($variable) -> $(unparse(value)))
        Primitive(:iff, [cond, true_branch, Constant(false)]) => :(if $(unparse(cond)) $(unparse(true_branch)) end)
        Primitive(:iff, [cond, true_branch, false_branch]) => :(if $(unparse(cond)) $(unparse(true_branch)) else $(unparse(false_branch)) end)
        Primitive(f, args) => :($(unparse(f))($(map(unparse, args)...)))
    end
end

stdenv = Dict{Symbol, Set}(
  :everything => Set{Any}([(scalar,) for scalar in [0, 1, 2, "alice", "bob", "eve", "cthulu", "yes", "no"]]),
  :nothing => Set([]),
  )
