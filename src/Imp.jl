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
  Base.Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

# --- parse ---

abstract type Expr end

struct Constant{T} <: Expr
    value::T # scalar
end

struct Var <: Expr
    name::Symbol
    scope::Int64 # vars with the same name in different scopes get different ids here
end

Var(name::Symbol) = Var(name, 0) # 0 == global scope

struct Apply <: Expr
    f::Expr
    args::Vector{Expr}
end

# things which operate on relations rather than on values
struct Primitive <: Expr
    f::Symbol
    args::Vector{Expr}
end

struct Abstract <: Expr
    vars::Vector{Var}
    value::Expr
end

@generated function Base.:(==)(a::T, b::T) where {T <: Expr}
    Base.Expr(:&&, @splice fieldname in fieldnames(T) quote
         (a.$fieldname == b.$fieldname)
         end)
end

@generated function map_expr(f, constructor, expr::Expr)
    quote
        constructor($(@splice fieldname in fieldnames(expr) begin
                      if fieldtype(expr, fieldname) <: Expr
                      :(f(expr.$fieldname))
                      elseif fieldtype(expr, fieldname) <: Vector{T} where {T <: Expr}
                      :(map(f, expr.$fieldname))
                      else
                      :(expr.$fieldname)
                      end
                      end))
    end
end
map_expr(f, expr::Expr) = map_expr(f, typeof(expr), expr)

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
    elseif @capture(ast, var_Symbol -> value_)
        Abstract([Var(var)], parse(value))
    elseif @capture(ast, (vars__,) -> value_)
        Abstract(map(Var, vars), parse(value))
    else
        error("Unknown syntax: $ast")
    end
end

function unparse(expr::Expr)
    @match (expr, map_expr(unparse, tuple, expr)) begin
        (_::Constant, (value,)) => value
        (_::Var, (name, _)) => name
        (_::Apply, (f, args)) => :($f($(args...)))
        (_::Primitive, (:iff, [cond, true_branch, Constant(false)])) => :(if $cond; $true_branch end)
        (_::Primitive, (:iff, [cond, true_branch, false_branch])) => :(if $cond; $true_branch else $false_branch end)
        (_::Primitive, (f, args)) => :($f($(args...)))
        (_::Abstract, ([var], value)) => :($var -> $value)
        (_::Abstract, (vars, value)) => :(($(vars...)) -> $value)
    end
end

Base.show(io::IO, expr::Expr) = print(io, string("@imp(", unparse(expr), ")"))

# --- analyze ---

struct CompileError
    message::String
end

struct Scope
    current::Dict{Symbol, Int64}
    used::Dict{Symbol, Int64}
end

Scope() = Scope(Dict(), Dict())
Scope(env::Dict{Var}) = Scope(Dict(var.name => 0 for var in keys(env)), Dict(var.name => 0 for var in keys(env)))

scopify(scope::Scope, expr::Expr) = map_expr((expr) -> scopify(scope, expr), expr)

function scopify(scope::Scope, expr::Var)
    id = get(scope.current, expr.name) do
        throw(CompileError(("Not in scope: $(expr.name)"))) # TODO how do we report location?
    end
    Var(expr.name, id)
end

function scopify(scope::Scope, expr::Abstract)
    scope = Scope(copy(scope.current), scope.used)
    for var in expr.vars
        scope.current[var.name] = scope.used[var.name] = get(scope.used, var.name, 0) + 1
    end
    map_expr((expr) -> scopify(scope, expr), expr)
end

# --- interpret ---

const Env{T} = Dict{Var, T}

function interpret(env::Env, expr::Var) ::Set
    env[expr]
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
        var_rows = Set()
        for var_row in env[Var(:everything)]
            env[var] = Set([var_row])
            for value_row in interpret(env, expr, var_ix+1)
                push!(result, (var_row..., value_row...))
                push!(var_rows, var_row)
            end
        end
        env[var] = var_rows # kind of ugly that we put this in just for typechecking
        result
    end
end

function interpret(env::Env, expr::Abstract)
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
        (:exists, [arg]) => bool_to_set(arg != env[Var(:nothing)])
        (:forall, [arg]) => bool_to_set(arg == env[Var(:everything)])
        _ => error("Unknown primitive: $expr")
    end
end

stdenv = Env{Set}(
  Var(:everything) => Set{Any}([(scalar,) for scalar in [0, 1, 2, "alice", "bob", "eve", "cthulu", "yes", "no"]]),
  Var(:nothing) => Set([]),
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
        (:!, [arg]) => bool_type # because we don't have a true_type
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
    arg::Var
    columns::Vector{Int64}
end

struct BoundedAbstract <: Expr
    var::Var
    upper_bound::Expr
    value::Expr
end

function unparse(expr::Union{Permute, BoundedAbstract})
    @match (expr, map_expr(unparse, tuple, expr)) begin
        (_::Permute, (arg, columns)) => :($arg[$(columns...)])
        (_::BoundedAbstract, (var, upper_bound, value)) => :(for $var in $upper_bound; $value; end)
    end
end

function upper_bound(bound_vars::Vector{Var}, var::Var, expr::Constant)::Expr
    (expr.value == false) ? Var(:nothing) : Var(:everything)
end

function upper_bound(bound_vars::Vector{Var}, var::Var, expr::Apply)::Expr
    @assert expr.f isa Var
    @assert all(expr.args) do arg
        arg isa Var
    end
    if !(var in expr.args) 
        Var(:everything)
    else
        apply_args = Var[]
        permute_columns = Int64[]
        for bound_var in bound_vars
            column = findfirst(expr.args, bound_var)
            if column != 0
                push!(apply_args, bound_var)
                push!(permute_columns, column)
            end
        end
        column = findfirst(expr.args, var) # TODO repeated vars?
        push!(permute_columns, column)
        Apply(Permute(expr.f, permute_columns), apply_args)
    end 
end

function todo(expr)
    @warn "Unimplemented: $expr"
    Var(:everything)
end

function upper_bound(bound_vars::Vector{Var}, var::Var, expr::Primitive)::Expr
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

function simplify_upper_bound(expr::Expr)
    @match map_expr(simplify_upper_bound, expr) begin
        Primitive(:|, [a && Var(:everything, _), b]) => a
        Primitive(:|, [a, b && Var(:everything, _)]) => b
        Primitive(:|, [a && Var(:nothing, _), b]) => b
        Primitive(:|, [a, b && Var(:nothing, _)]) => a
        Primitive(:&, [a && Var(:everything, _), b]) => b
        Primitive(:&, [a, b && Var(:everything, _)]) => a
        Primitive(:&, [a && Var(:nothing, _), b]) => a
        Primitive(:&, [a, b && Var(:nothing, _)]) => b
        Primitive(:!, [Var(:nothing, _)]) => Var(:everything)
        Primitive(:!, [Var(:everything, _)]) => Var(:nothing)
        other => other
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

function imp(ast)
    scopify(Scope(), parse(ast))
end

function imp(env, ast)
    scopify(Scope(env), parse(ast))
end

function imp!(env, ast)
    interpret(env, imp(env, ast))
end

macro imp(ast)
    :(imp($(QuoteNode(ast))))
end

macro imp(env, ast)
    :(imp($(esc(env)), $(QuoteNode(ast))))
end

macro imp!(env, ast)
    :(imp!($(esc(env)), $(QuoteNode(ast))))
end

export stdenv, imp, imp!, @imp, @imp!, interpret, infer

end
