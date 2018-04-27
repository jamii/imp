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

@generated function Base.hash(expr::Expr, h::UInt)
    value = :h
    for fieldname in fieldnames(expr)
        value = :(hash(expr.$fieldname, $value))
    end
    value
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
        elseif name == :(nothing)
            Constant(false)
        else
            Var(name)
        end
    elseif @capture(ast, f_(args__))
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
        (_::Primitive, (f, args)) => :($f($(args...),))
        (_::Abstract, ([var], value)) => :($var -> $value)
        (_::Abstract, (vars, value)) => :(($(vars...),) -> $value)
    end
end

Base.show(io::IO, expr::Expr) = print(io, string("@imp(", unparse(expr), ")"))

# --- analyze ---

struct CompileError
    message::String
end

compile_error(message::String) = throw(CompileError(message))

struct Scope
    current::Dict{Symbol, Int64}
    used::Dict{Symbol, Int64}
end

Scope() = Scope(Dict(), Dict())
Scope(env::Dict{Var}) = Scope(Dict(var.name => 0 for var in keys(env)), Dict(var.name => 0 for var in keys(env)))

separate_scopes(scope::Scope, expr::Expr) = map_expr((expr) -> separate_scopes(scope, expr), expr)

function separate_scopes(scope::Scope, expr::Var)
    id = get(scope.current, expr.name) do
        compile_error("Not in scope: $(expr.name)") # TODO how do we report location?
    end
    Var(expr.name, id)
end

function separate_scopes(scope::Scope, expr::Abstract)
    scope = Scope(copy(scope.current), scope.used)
    for var in expr.vars
        scope.current[var.name] = scope.used[var.name] = get(scope.used, var.name, 0) + 1
    end
    map_expr((expr) -> separate_scopes(scope, expr), expr)
end

# --- interpret ---

const Env{T} = Dict{Var, T}

function interpret(env::Env, expr::Expr) ::Set
    _interpret(env, expr)
end

function _interpret(env::Env, expr::Var) ::Set
    env[expr]
end

function _interpret(env::Env, expr::Apply) ::Set
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

function _interpret(env::Env, expr::Abstract, var_ix::Int64) ::Set
    if var_ix > length(expr.vars)
        interpret(env, expr.value)
    else
        var = expr.vars[var_ix]
        result = Set()
        for var_row in env[Var(:everything)]
            env[var] = Set([var_row])
            for value_row in _interpret(env, expr, var_ix+1)
                push!(result, (var_row..., value_row...))
            end
        end
        result
    end
end

function _interpret(env::Env, expr::Abstract)
    _interpret(env, expr, 1)
end

# --- interpret values ---

const truthy = Set([()])
const falsey = Set()
bool_to_set(bool::Bool)::Set = bool ? truthy : falsey
set_to_bool(set::Set)::Bool = length(set) > 0

function _interpret(env::Env{Set}, expr::Constant) ::Set
    Set([(expr.value,)])
end

function _interpret(env::Env{Set}, expr::Constant{Bool}) ::Set
    bool_to_set(expr.value)
end

function _interpret(env::Env{Set}, expr::Primitive) ::Set
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
        (:exists, [arg]) => bool_to_set(arg != falsey)
        (:forall, [arg]) => bool_to_set(arg == env[Var(:everything)])
        _ => error("Unknown primitive: $expr")
    end
end

# --- interpret types ---

const RowType = NTuple{N, Type} where N
const SetType = Set{RowType}

const bool_type = SetType([()])
const false_type = SetType([])

function _interpret(env::Env{SetType}, expr::Constant{T})::SetType where T
    Set([(T,)])
end

function _interpret(env::Env{SetType}, expr::Constant{Bool})::SetType where T
    expr.value ? bool_type : false_type
end

function _interpret(env::Env{SetType}, expr::Primitive)::SetType
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
        (:exists, [arg]) => arg == false_type ? false_type : bool_type
        (:forall, [arg]) => bool_type
        _ => error("Unknown primitive: $expr")
    end
end

row_type(row::Tuple) = map(typeof, row)
set_type(set::Set) = map(row_type, set)
env_types(env::Env{Set}) = Env{SetType}(name => set_type(set) for (name, set) in env)

# TODO totally gross global, pass a context instead
const expr_types = Dict{Expr, SetType}()
function interpret(env::Env{SetType}, expr::Expr)
    expr_type = _interpret(env, expr)
    # TODO really we want to union types of vars in their abstract, much more than the type of the expression
    union!(get!(expr_types, expr, SetType()), expr_type)
    expr_type
end
function infer_types(env::Env{Set}, expr::Expr)::Dict{Expr, SetType}
    empty!(expr_types)
    interpret(env_types(env), expr)
    copy(expr_types)
end

# --- lower ---

# TODO should probably replace anything that has false_type with false

const Arity = Union{Int64, Nothing} # false has arity nothing

function arity(set_type::SetType)::Arity
    arities = map(length, set_type)
    @match length(arities) begin
        0 => nothing
        1 => first(arities)
        _ => compile_error("Ill-typed: $set_type")
    end
end

function replace_expr(expr::Expr, replacements::Dict)::Expr
    new_expr = haskey(replacements, expr) ? replacements[expr] : expr
    map_expr(expr -> replace_expr(expr, replacements), new_expr)
end

# rewrite until every Apply has var/constant f, boolean type and bound var args
function simple_apply(arity::Dict{Expr, Arity}, expr::Expr)::Expr
    arity = copy(arity)
    last_id = Ref(0)
    make_slots(n) = begin
        slots = [Var(Symbol("_$(last_id[] += 1)"), 1) for _ in 1:n]
        for slot in slots
            arity[slot] = 1
        end
        slots
    end
    apply(expr::Expr) = @match arity[expr] begin
        nothing => Imp.Constant(false) # if it's provable false, why bother keeping it around?
        n => begin
            slots = make_slots(n)
            Abstract(slots, apply(expr, slots))
        end
    end
    apply(expr::Expr, slots::Vector{Var})::Expr = @match expr begin

        # false[slots...] => false
        Constant{Bool}(false) => expr
        
        # true[] => true
        Constant{Bool}(true) => begin
            @match [] = slots
            expr
        end
        
        # 1[slot] => slot==1
        _::Constant => begin
            @match [slot] = slots
            Primitive(:(==), [slot, expr])
        end
        
        # f[slots...] => f(slots...)
        Var(_, 0) =>  Apply(expr, slots)
        
        # x[slot] => slot==x
        Var(_, _) => begin
            @match [slot] = slots
            Primitive(:(==), [slot, expr])
        end
        
        # f(x,y)[slots...] => f(x)(y)[slots...]
        Apply(f, []) => apply(f, slots)
        Apply(f, args) where (length(args) > 1) => apply(reduce((f, arg) -> Apply(f, [arg]), f, args), slots)
        
        # f(x)[slots...] => f[x, slots...]
        Apply(f, [x && Var(_, scope)]) where (scope != 0) => apply(f, [x, slots...])
        
        # f(g)[slots...] => exists((new_slots...) -> g[new_slots...] & f[new_slots..., slots...])
        Apply(f, [g]) => begin
            new_slots = make_slots(arity[g])
            Primitive(:exists,
                      [Abstract(new_slots,
                                Primitive(:&, [
                                    apply(g, new_slots),
                                    apply(f, vcat(new_slots, slots))
                                ]))])
        end

        # things which produce bools
        # (a == b)[] => (a[...] == b[...])
        Primitive(f, args) where (f in [:!, :(=>), :(==), :exists, :forall]) => begin
            @match [] = slots
            Primitive(f, map(apply, args))
        end

        # things which produce scalars
        # reduce(...)[slot] => slot==reduce(...)
        Primitive(:reduce, _) => begin
            @match [slot] = slots
            Primitive(:(==), [slot, expr])
        end

        # things which produce sets
        # (a | b)[slots...] => (a[slots...] | b[slots...])
        Primitive(:|, [a,b]) => Primitive(:|, [apply(a, slots), apply(b, slots)])
        Primitive(:&, [a,b]) => Primitive(:&, [apply(a, slots), apply(b, slots)])
        Primitive(:iff, [cond, true_branch, false_branch]) => Primitive(:iff, [cond, apply(true_branch, slots), apply(false_branch, slots)])

        # ((vars...) -> value)[slots...] => value[vars... = slots...]
        Abstract(vars, value) => begin
            n = length(vars)
            @assert n <= length(slots)
            replace_expr(apply(value, slots[n+1:end]), Dict(zip(vars, slots[1:n])))
        end
    end
    apply(expr)
end

# TODO can remove this if we switch to 1-arg abstract
function simple_abstract(expr::Expr)
    expr = map_expr(simple_abstract, expr)
    while true
        expr = @match expr begin
            Abstract([], value) => value
            Abstract(vars1, Abstract(vars2, value)) => Abstract(vcat(vars1, vars2), value)
            _ => return expr
        end
    end
end 

function lower_apply(expr_types::Dict{Expr, SetType}, expr::Expr)::Expr
    arities = Dict{Expr, Arity}(((expr, arity(set_type)) for (expr, set_type) in expr_types))
    expr = simple_apply(arities, expr)
    expr = simple_abstract(expr)
    expr
end

# --- bounded abstract ----

# need Abstract over fully lowered value of arity 0

# TODO might want to permute stuff so we can hit vars earlier? does it matter?

struct Permute <: Expr
    arg::Var
    columns::Vector{Int64}
end

struct BoundedAbstract <: Expr
    var::Var
    bound::Expr
    value::Expr
end

function unparse(expr::Union{Permute, BoundedAbstract})
    @match (expr, map_expr(unparse, tuple, expr)) begin
        (_::Permute, (arg, columns)) => :($arg[$(columns...)])
        (_::BoundedAbstract, (var, upper_bound, value)) => :(for $var in $upper_bound; $value; end)
    end
end

function todo(expr)
    @warn "Unimplemented: $expr"
    Var(:everything)
end

function simplify_bound(expr::Expr)
    @match map_expr(simplify_bound, expr) begin
        Primitive(:|, [a && Var(:everything, 0), b]) => a
        Primitive(:|, [a, b && Var(:everything, 0)]) => b
        Primitive(:|, [a && Constant(false), b]) => b
        Primitive(:|, [a, b && Constant(false)]) => a
        Primitive(:&, [a && Var(:everything, 0), b]) => b
        Primitive(:&, [a, b && Var(:everything, 0)]) => a
        Primitive(:&, [a && Constant(false), b]) => a
        Primitive(:&, [a, b && Constant(false)]) => b
        Primitive(:!, [Constant(false)]) => Var(:everything)
        Primitive(:!, [Var(:everything, 0)]) => Constant(false)
        other => other
    end
end

function bound(bound_vars::Vector{Var}, var::Var, expr::Constant)::Expr
    (expr.value == false) ? Constant(false) : Var(:everything)
end

function bound(bound_vars::Vector{Var}, var::Var, expr::Abstract)::Expr
    compile_error("Should have been lowered: $expr")
end

function bound(bound_vars::Vector{Var}, var::Var, expr::Abstract)::Expr
    bound(bound_vars, var, expr.value)
end

function bound(bound_vars::Vector{Var}, var::Var, expr::Apply)::Expr
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

function bound(bound_vars::Vector{Var}, var::Var, expr::Primitive)::Expr
    @match (expr.f, expr.args) begin
        (:|, [a, b]) => Primitive(:|, [bound(bound_vars, var, a), bound(bound_vars, var, b)])
        (:&, [a, b]) => Primitive(:&, [bound(bound_vars, var, a), bound(bound_vars, var, b)])
        (:!, [arg]) => todo(expr)
        (:(=>), [a, b]) => todo(expr)
        (:(==), [a, b]) => todo(expr)
        # branches must be boolean at this point
        (:iff, [cond, true_branch, false_branch]) => bound(bound_vars, var, Primitive(:|, [Primitive(:&, [cond, true_branch]), false_branch]))
        (:reduce, [raw_op, raw_init, values]) => todo(expr)
        (:exists, [arg]) => bound(bound_vars, var, arg)
        (:forall, [arg]) => todo(expr)
        _ => error("Unknown primitive: $expr")
    end
end

bound_abstract(expr::Expr)::Expr = map_expr(bound_abstract, expr)
function bound_abstract(expr::Abstract)::Expr
    # TODO need to also remove redundant computation from value
    value = bound_abstract(expr.value)
    for i in reverse(1:length(expr.vars))
        var_bound = bound(expr.vars[1:i-1], expr.vars[i], expr.value)
        var_bound = simplify_bound(var_bound)
        value = BoundedAbstract(expr.vars[i], var_bound, value)
    end
    value
end

function _interpret(env::Env, expr::Permute)::Set
    Set((row[expr.columns] for row in env[expr.arg]))
end

function _interpret(env::Env, expr::BoundedAbstract)::Set
    bound = interpret(env, expr.bound)
    result = Set()
    for var_row in bound
        env[expr.var] = Set([var_row])
        for value_row in interpret(env, expr.value)
            push!(result, (var_row..., value_row...))
        end
    end
    result
end

# --- exports ---

const all_passes = [:parse, :separate_scopes, :infer_types, :lower_apply, :bound_abstract, :interpret]

function imp(expr; globals=Dict{Symbol, Set}(), everything=nothing, passes=all_passes)
    env = Env{Set}(Var(name) => set for (name, set) in globals)
    if everything != nothing
        env[Var(:everything)] = everything
    end
    if :parse in passes
        expr = parse(expr)
    end
    if :separate_scopes in passes
        expr = separate_scopes(Scope(env), expr)
    end
    if :infer_types in passes
        inferred_types = infer_types(env, expr)
    end
    if :lower_apply in passes
        expr = lower_apply(inferred_types, expr)
    end
    if :bound_abstract in passes
        expr = bound_abstract(expr)
    end
    if :interpret in passes
        expr = interpret(env, expr)
    end
    expr
end

export imp

end
