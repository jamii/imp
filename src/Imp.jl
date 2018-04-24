module Imp

import MacroTools
import MacroTools: @capture
using Rematch

# TODO remove workaround for https://github.com/JuliaLang/julia/issues/26885
function Base.show(io::IO, set::Set)
    print(io, "Set(", collect(set), ")")
end

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

# --- parse ---

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
    vars::Vector{Symbol}
    value::Expr
end

# things which operate on relations rather than on values
struct Primitive <: Expr
    f::Symbol
    args::Vector{Expr}
end

function parse(ast)
    if @capture(ast, constant_Int64_String_Bool)
        Constant(constant)
    elseif @capture(ast, name_Symbol)
        if name == :(_)
            Var(:everything)
        else
            Var(name)
        end
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
    elseif @capture(ast, (var_Symbol) -> value_)
        Abstract([var], parse(value))
    elseif @capture(ast, (vars__,) -> value_)
        Abstract(vars, parse(value))
    else
        error("Unknown syntax: $ast")
    end
end

function unparse(expr)
    @match expr begin
        Constant(value) => value
        Var(name) => name
        Apply(f, args) => :($(unparse(f))($(map(unparse, args)...)))
        Abstract(vars, value) => :(($(vars...)) -> $(unparse(value)))
        Primitive(:iff, [cond, true_branch, Constant(false)]) => :(if $(unparse(cond)) $(unparse(true_branch)) end)
        Primitive(:iff, [cond, true_branch, false_branch]) => :(if $(unparse(cond)) $(unparse(true_branch)) else $(unparse(false_branch)) end)
        Primitive(f, args) => :($(unparse(f))($(map(unparse, args)...)))
    end
end

# --- interpret ---

const Env{T} = Dict{Symbol, T}

function interpret(env::Env, expr::Var) ::Set
    env[expr.name]
end

function interpret(env::Env, expr::Apply) ::Set
    f = interpret(env, expr.f)
    for arg in map((arg) -> interpret(env, arg), expr.args)
        result = Set()
        for n in map(length, arg)
            for row in f
                if (length(row) >= n) && (row[1:n] in arg)
                    push!(result, row[n+1:end])
                end
            end
        end
        f = result
    end
    f
end

function interpret(env::Env, expr::Abstract, var_ix::Int64) ::Set
    if var_ix > length(expr.vars)
        interpret(env, expr.value)
    else
        var = expr.vars[var_ix]
        result = Set()
        for var_row in env[:everything]
            env[var] = Set([var_row])
            for value_row in interpret(env, expr, var_ix+1)
                push!(result, (var_row..., value_row...))
            end
        end
        result
    end
end

function interpret(env::Env, expr::Abstract)
    env = copy(env)
    interpret(env, expr, 1)
end

# --- interpret values ---

const truthy = Set([()])
const falsey = Set()
bool_to_set(bool::Bool)::Set = bool ? truthy : falsey
set_to_bool(set::Set)::Bool = length(set) > 0

function interpret(env::Env{Set}, expr::Constant) ::Set
    Set([(expr.value,)])
end

function interpret(env::Env{Set}, expr::Constant{Bool}) ::Set
    bool_to_set(expr.value)
end

function interpret(env::Env{Set}, expr::Primitive) ::Set
    args = [interpret(env, arg) for arg in expr.args]
    @match (expr.f, args) begin
        (:|, [a, b]) => union(a,b)
        (:&, [a, b]) => intersect(a,b)
        (:!, [arg]) => bool_to_set(!set_to_bool(arg))
        (:(=>), [a, b]) => bool_to_set((!set_to_bool(a) || set_to_bool(b)))
        (:(==), [a, b]) => bool_to_set(a == b)
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

# --- interpret types ---

const RowType = NTuple{N, Type} where N
const SetType = Set{RowType}

const bool_type = SetType([()])
const false_type = SetType([])

function interpret(env::Env{SetType}, expr::Constant{T})::SetType where T
    Set([(T,)])
end

function interpret(env::Env{SetType}, expr::Constant{Bool})::SetType where T
    expr.value ? bool_type : false_type
end

function interpret(env::Env{SetType}, expr::Primitive)::SetType
    arg_types = [interpret(env, arg) for arg in expr.args]
    @match (expr.f, arg_types) begin
        (:|, [a, b]) => union(a, b)
        (:&, [a, b]) => intersect(a, b)
        (:!, [arg]) => bool_type
        (:(=>), [a, b]) => bool_type
        (:(==), [a, b]) => intersect(a, b) == false_type ? false_type : bool_type
        (:iff, [cond_type, then_type, else_type]) => begin
            # TODO if we had lower bounds on cond we could also do then_type when it's definitely true 
            if cond_type == false_type
                else_type
            else
                union(then_type, else_type)
            end
        end
        (:reduce, [raw_op, raw_init, values]) => map(row_type -> (row_type[3],), raw_op)
        (:exists, [arg]) => bool_type
        (:forall, [arg]) => bool_type
        _ => error("Unknown primitive: $expr")
    end
end

row_type(row::Tuple) = map(typeof, row)
set_type(set::Set) = map(row_type, set)
env_types(env::Env{Set}) = Env{SetType}(name => set_type(set) for (name, set) in env)
infer(env::Env{Set}, expr::Expr) = interpret(env_types(env), expr)

# --- bounded abstract ----

# need Abstract over fully lowered value of arity 0

struct Permute <: Expr
    arg::Symbol
    columns::Vector{Int64}
end

struct BoundedAbstract <: Expr
    var::Symbol
    upper_bound::Expr
    value::Expr
end

# TODO unparse

function upper_bound(bound_vars::Vector{Symbol}, var::Symbol, expr::Constant)::Expr
    (expr.value == false) ? Var(:nothing) : Var(:everything)
end

function upper_bound(bound_vars::Vector{Symbol}, var::Symbol, expr::Apply)::Expr
    @assert expr.f isa Var
    f = expr.f.name
    args = map(expr.args) do arg
        @assert arg isa Var
        arg.name
    end
    if !(var in args) 
        Var(:everything)
    else
        apply_args = Var[]
        permute_columns = Int64[]
        for bound_var in bound_vars
            column = findfirst(args, bound_var)
            if column != 0
                push!(apply_args, Var(bound_var))
                push!(permute_columns, column)
            end
        end
        column = findfirst(args, var) # TODO repeated vars?
        push!(permute_columns, column)
        Apply(Permute(f, permute_columns), apply_args)
    end 
end

function todo(expr)
    @warn "Unimplemented: $expr"
    Var(:everything)
end

function upper_bound(bound_vars::Vector{Symbol}, var::Symbol, expr::Primitive)::Expr
    @match (expr.f, expr.args) begin
        (:|, [a, b]) => Primitive(:|, [upper_bound(bound_vars, var, a), upper_bound(bound_vars, var, b)])
        (:&, [a, b]) => Primitive(:&, [upper_bound(bound_vars, var, a), upper_bound(bound_vars, var, b)])
        (:!, [arg]) => todo(expr)
        (:(=>), [a, b]) => todo(expr)
        (:(==), [a, b]) => todo(expr)
        # branches must be boolean at this point
        (:iff, [cond, true_branch, false_branch]) => upper_bound(bound_vars, var, Primitive(:|, [Primitive(:&, [cond, true_branch]), false_branch]))
        (:reduce, [raw_op, raw_init, values]) => todo(expr)
        (:exists, [arg]) => upper_bound(bound_vars, var, arg)
        (:forall, [arg]) => todo(expr)
        _ => error("Unknown primitive: $expr")
    end
end

function simplify_upper_bound(expr::Expr)::Expr
    s(expr::Expr) = @match expr begin
        Primitive(:|, [a && Var(:everything), b]) => s(a)
        Primitive(:|, [a, b && Var(:everything)]) => s(b)
        Primitive(:|, [a && Var(:nothing), b]) => s(b)
        Primitive(:|, [a, b && Var(:nothing)]) => s(a)
        Primitive(:&, [a && Var(:everything), b]) => s(b)
        Primitive(:&, [a, b && Var(:everything)]) => s(a)
        Primitive(:&, [a && Var(:nothing), b]) => s(a)
        Primitive(:&, [a, b && Var(:nothing)]) => s(b)
        Primitive(:!, [Var(:nothing)]) => Var(:everything)
        Primitive(:!, [Var(:everything)]) => Var(:nothing)
        _ => expr
    end
    @show @match expr begin
        _::Var => expr
        _::Apply => expr
        Primitive(f, args) => s(Primitive(f, map(s, args)))
    end
end     

function with_upper_bound(expr::Abstract)
    value = expr.value # TODO once bounds are precise can do value = true
    for i in reverse(1:length(expr.vars))
        bound = upper_bound(expr.vars[1:i-1], expr.vars[i], expr.value)
        bound = simplify_upper_bound(bound)
        value = BoundedAbstract(expr.vars[i], bound, value)
    end
    value
end

function interpret(env::Env, expr::Permute)::Set
    Set((row[expr.columns] for row in env[expr.arg]))
end

function interpret(env::Env, expr::BoundedAbstract)::Set
    upper_bound = interpret(env, expr.upper_bound)
    result = Set()
    for var_row in upper_bound
        env[expr.var] = Set([var_row])
        for value_row in interpret(env, expr.value)
            push!(result, (var_row..., value_row...))
        end
    end
    result
end

# --- exports ---

macro imp(ast)
    :(parse($(QuoteNode(ast))))
end

macro imp(env, ast)
    :(interpret($(esc(env)), parse($(QuoteNode(ast)))))
end

export stdenv, @imp, interpret, infer

end
