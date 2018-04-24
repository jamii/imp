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
    variable::Symbol
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

# --- interpret ---

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

stdenv = Dict{Symbol, Set}(
  :everything => Set{Any}([(scalar,) for scalar in [0, 1, 2, "alice", "bob", "eve", "cthulu", "yes", "no"]]),
  :nothing => Set([]),
)

# --- infer ---

const RowType = NTuple{N, Type} where N
const SetType = Set{RowType}
const EnvTypes = Dict{Symbol, SetType}

const universe_types = Set([Int64, Float64, String])
const bool_type = SetType([()])
const false_type = SetType([])

function infer(env_types::EnvTypes, expr::Constant{T})::SetType where T
    Set([(T,)])
end

function infer(env_types::EnvTypes, expr::Constant{Bool})::SetType where T
    expr.value ? bool_type : false_type
end

function infer(env_types::EnvTypes, expr::Var)::SetType
    env_types[expr.name]
end

function infer(env_types::EnvTypes, expr::Apply)::SetType
    f_type = infer(env_types, expr.f)
    for arg_type in map((arg) -> infer(env_types, arg), expr.args)
        result_type = SetType()
        for f_row_type in f_type
            for arg_row_type in arg_type
                if length(arg_row_type) <= length(f_row_type) &&
                    arg_row_type == f_row_type[1:length(arg_row_type)]
                    push!(result_type, f_row_type[length(arg_row_type)+1:end])
                end
            end
        end
        f_type = result_type
    end
    f_type
end

function infer(env_types::EnvTypes, expr::Abstract)::SetType
    result_type = SetType()
    env_types = copy(env_types)
    for variable_type in universe_types
        env_types[expr.variable] = SetType([(variable_type,)])
        for value_type in infer(env_types, expr.value)
            push!(result_type, (variable_type, value_type...))
        end
    end
    result_type
end

function infer(env_types::EnvTypes, expr::Primitive)::SetType
    arg_types = [infer(env_types, arg) for arg in expr.args]
    @match (expr.f, arg_types) begin
        (:|, _) => union(arg_types...)
        (:&, _) => intersect(arg_types...)
        (:!, [arg]) => bool_type
        (:(=>), [a, b]) => bool_type
        (:(==), _) => intersect(arg_types...) == false_type ? false_type : bool_type
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

# --- exports ---

macro imp(ast)
    parse(ast)
end

macro imp(env, ast)
    :(interpret($(esc(env)), parse($(QuoteNode(ast)))))
end

export stdenv, @imp, interpret, infer

end
