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

struct False <: Expr end
struct True <: Expr end

struct Constant <: Expr
    value # scalar
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

struct Let <: Expr
    var::Var
    value::Expr
    body::Expr
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
    if @capture(ast, bool_Bool)
        bool ? True() : False()
    elseif @capture(ast, constant_Int64_String)
        Constant(constant)
    elseif @capture(ast, name_Symbol)
        if name == :(_)
            Var(:everything)
        elseif name == :(nothing)
            False()
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
        Primitive(:iff, [parse(cond), parse(true_branch), False()])
    elseif @capture(ast, if cond_ true_branch_ else false_branch_ end)
        Primitive(:iff, [parse(cond), parse(true_branch), parse(false_branch)])
    elseif @capture(ast, var_Symbol -> value_)
        Abstract([Var(var)], parse(value))
    elseif @capture(ast, (vars__,) -> value_)
        Abstract(map(Var, vars), parse(value))
    elseif @capture(ast, let begin bindings__ end; body_ end)
        reduce(parse(body), bindings) do body, binding
            @assert @capture(binding, var_Symbol = value_) "Unknown syntax: $ast"
            Let(Var(var), parse(value), body)
        end
    elseif @capture(ast, (exprs__,))
        Primitive(:tuple, map(parse, exprs))
    else
        error("Unknown syntax: $ast")
    end
end

function unparse(expr::Expr)
    @match (expr, map_expr(unparse, tuple, expr)) begin
        (_::True, ()) => true
        (_::False, ()) => false
        (_::Constant, (value,)) => value
        (_::Var, (name, _)) => name
        (_::Apply, (f, args)) => :($f($(args...)))
        (_::Primitive, (:iff, [cond, true_branch, False()])) => :(if $cond; $true_branch end)
        (_::Primitive, (:iff, [cond, true_branch, false_branch])) => :(if $cond; $true_branch else $false_branch end)
        (_::Primitive, (:tuple, args)) => :(($(args...),))
        (_::Primitive, (f, args)) => :($f($(args...),))
        (_::Abstract, ([var], value)) => :($var -> $value)
        (_::Abstract, (vars, value)) => :(($(vars...),) -> $value)
        (_::Let, (var, value, body)) => :(let $var = $value; $body end)
    end
end

Base.show(io::IO, expr::Expr) = print(io, string("@imp(", unparse(expr), ")"))

# --- scoping ---

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

function separate_scopes(scope::Scope, expr::Let)
    # expr.var is not in scope during expr.value
    value = separate_scopes(scope, expr.value)
    scope = Scope(copy(scope.current), scope.used)
    scope.current[expr.var.name] = scope.used[expr.var.name] = get(scope.used, expr.var.name, 0) + 1
    var = separate_scopes(scope, expr.var)
    body = separate_scopes(scope, expr.body)
    Let(var, value, body)
end

# --- interpret ---

const Env{T} = Dict{Var, T}

function interpret(env::Env, expr::Expr)::Set
    _interpret(env, expr)
end

function _interpret(env::Env, expr::Var)::Set
    env[expr]
end

function _interpret(env::Env, expr::Apply)::Set
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

function _interpret(env::Env, expr::Abstract, var_ix::Int64)::Set
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

function _interpret(env::Env, expr::Abstract)::Set
    _interpret(env, expr, 1)
end

function _interpret(env::Env, expr::Let)::Set
    env[expr.var] = interpret(env, expr.value)
    interpret(env, expr.body)
end

# --- interpret values ---

const false_set = Set()
const true_set = Set([()])
bool_to_set(bool::Bool)::Set = bool ? true_set : false_set
set_to_bool(set::Set)::Bool = !isempty(set)

_interpret(env::Env{Set}, expr::False) = false_set
_interpret(env::Env{Set}, expr::True) = true_set

function _interpret(env::Env{Set}, expr::Constant) ::Set
    Set([(expr.value,)])
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
        (:exists, [arg]) => bool_to_set(arg != false_set)
        (:forall, [arg]) => bool_to_set(arg == env[Var(:everything)])
        (:tuple, args) => reduce(true_set, args) do a, b
            Set(((a_row..., b_row...) for a_row in a for b_row in b))
        end
        _ => error("Unknown primitive: $expr")
    end
end

# --- interpret types ---

const RowType = NTuple{N, Type} where N
const SetType = Set{RowType}

const bool_type = SetType([()])
const false_type = SetType([])

_interpret(env::Env{SetType}, expr::True) = bool_type
_interpret(env::Env{SetType}, expr::False) = false_type

function _interpret(env::Env{SetType}, expr::Constant)::SetType
    Set([(typeof(expr.value),)])
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
        (:tuple, args) => reduce(true_set, args) do a, b
            Set(((a_row..., b_row...) for a_row in a for b_row in b))
        end
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

function desugar(arity::Dict{Expr, Arity}, last_id::Ref{Int64}, expr::Expr)::Expr
    desugar(expr) = @match expr begin
        Let(var, value, body) => replace_expr(desugar(body), Dict(var => desugar(value)))
        Primitive(:tuple, args) => begin
            vars = Var[]
            body = reduce(True(), args) do body, arg
                arg_vars = [Var(Symbol("_$(last_id[] += 1)"), 1) for _ in 1:coalesce(arity[arg], 0)]
                append!(vars, arg_vars)
                Primitive(:&, [body, Apply(arg, arg_vars)])
            end
            Abstract(vars, body)
        end
        _ => map_expr(desugar, expr)
    end
    desugar(expr)
end

function is_scalar(expr::Expr)
    @match expr begin
        _::Constant => true
        Var(_, 0) => false
        Var(_, _) => true
        _ => false
    end
end

# rewrite until every Apply has var/constant f and boolean type and all args are bound vars or _
function simple_apply(arity::Dict{Expr, Arity}, last_id::Ref{Int64}, expr::Expr)::Expr
    make_slots(n) = begin
        slots = [Var(Symbol("_$(last_id[] += 1)"), 1) for _ in 1:n]
        for slot in slots
            arity[slot] = 1
        end
        slots
    end
    apply(expr::Expr) = @match arity[expr] begin
        nothing => False() # if it's provable false, why bother keeping it around?
        n => begin
            slots = make_slots(n)
            Abstract(slots, apply(expr, slots))
        end
    end
    apply(expr::Expr, slots::Vector{Var})::Expr = @match expr begin

        # things that don't make sense will be hard to lower
        _ where get(arity, expr, -1) == nothing => False()

        # false[slots...] => false
        False() => expr

        # true[] => true
        True() => begin
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
        Apply(f, [x && Var(name, scope)]) where ((name == :(everything)) || (scope != 0)) => apply(f, [x, slots...])

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

        # (a & b)[slots...] => a[slots...] & b[slots...]
        Primitive(:&, [a, b]) => Primitive(:&, [apply(a, slots), apply(b, slots)])

        # (a | b)[slots...] => a[slots...] & b[slots...]
        Primitive(:|, [a, b]) => Primitive(:|, [apply(a, slots), apply(b, slots)])

        # (!a)[] => !(a[...])
        Primitive(:!, [arg]) => Primitive(:!, [apply(arg)])

        # (a => b)[] => b[] | !a[]
        Primitive(:(=>), [a, b]) => begin
            @match [] = slots
            Primitive(:|, [Primitive(:!, [apply(a)]), apply(b)])
        end

        # leave scalar eq alone, otherwise lower isn't idempotent
        Primitive(:(==), [a, b]) where (is_scalar(a) && is_scalar(b)) => expr
        # (a == b)[] => (a[...] == b[...])
        Primitive(:(==), [a, b]) => Primitive(:(==), [apply(a), apply(b)])

        # (c ? t : f)[slots...] => (c[...] & t[slots...]) | (!c[...] & f[slots...])
        Primitive(:iff, [c, t, f]) => Primitive(:|,
                                                [Primitive(:&, [Primitive(:exists, [apply(c)]), apply(t, slots)]),
                                                 Primitive(:&, [Primitive(:!, [apply(c)]), apply(f, slots)])])

        # reduce(...)[slot] => slot==reduce(...)
        Primitive(:reduce, _) => begin
            @match [slot] = slots
            Primitive(:(==), [slot, expr])
        end

        # E(arg)[] => E(arg[...])
        Primitive(:exists, [arg]) => Primitive(:exists, [apply(arg)])

        # something is wonky with negation - shouldn't need to do two reduction steps together
        # A(arg)[] => !(slot -> !arg[slot])
        Primitive(:forall, [arg]) => begin
            new_slots = make_slots(arity[arg])
            Primitive(:!, [Abstract(new_slots, Primitive(:!, [apply(arg, new_slots)]))])
        end

        # ((vars...) -> value)[slots...] => value[vars... = slots...]
        Abstract(vars, value) => begin
            n = length(vars)
            @assert n <= length(slots)
            replace_expr(apply(value, slots[n+1:end]), Dict(zip(vars, slots[1:n])))
        end
    end
    apply(expr)
end

function negate(expr::Expr)::Expr
    @match expr begin
        False() => True()
        True() => False()
        Constant(_) => False()
        # abstract vars can't be empty
        Var(_, scope) where scope > 0 => False()
        Primitive(:|, [a, b]) => Primitive(:&, [negate(a), negate(b)])
        Primitive(:&, [a, b]) => Primitive(:|, [negate(a), negate(b)])
        Primitive(:!, [arg]) => Primitive(:exists, [lower_negation(arg)])
        # TODO reduce?
        Primitive(:exists, [arg]) => negate(arg)
        Abstract([], value) => Abstract([], negate(value))
        _ => Primitive(:!, [lower_negation(expr)])
    end
end

function lower_negation(expr::Expr)::Expr
    @match expr begin
        Primitive(:!, [arg]) => negate(arg)
        _ => map_expr(lower_negation, expr)
    end
end

# DNF
# TODO creates duplicate vars - does that need to be fixed?
function raise_union(expr::Expr)::Expr
    raise(expr) = @match expr begin
        Abstract(vars, Primitive(:|, [a, b])) => Primitive(:|, [raise(Abstract(vars, a)), raise(Abstract(vars, b))])
        Primitive(:&, [Primitive(:|, [a1, a2]), b]) => Primitive(:|, [raise(Primitive(:&, [a1, b])), raise(Primitive(:&, [a2, b]))])
        Primitive(:&, [a, Primitive(:|, [b1, b2])]) => Primitive(:|, [raise(Primitive(:&, [a, b1])), raise(Primitive(:&, [a, b2]))])
        Primitive(:exists, [Primitive(:|, [a, b])]) => Primitive(:|, [raise(Primitive(:exists, [a])), raise(Primitive(:exists, [b]))])
        _ => expr
    end
    raise(map_expr(raise_union, expr))
end

function lower(env::Env{Set}, expr::Expr)::Expr
    last_id = Ref(0)

    expr_types = infer_types(env, expr)
    arities = Dict{Expr, Arity}(((expr, arity(set_type)) for (expr, set_type) in expr_types))
    expr = desugar(arities, last_id, expr)

    # TODO gross that we have to do type inference twice - that global!
    expr_types = infer_types(env, expr)
    arities = Dict{Expr, Arity}(((expr, arity(set_type)) for (expr, set_type) in expr_types))
    expr = simple_apply(arities, last_id, expr)

    expr = lower_negation(expr)
    expr = raise_union(expr)

    # TODO if we want to change variable ordering, need a pass that lifts exists here

    expr
end

# --- bounded abstract ----

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

function permute(bound_vars::Vector{Var}, var::Var, expr::Apply)::Expr
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

function is_scalar_bound(bound_vars::Vector{Var}, expr::Expr)
    @match expr begin
        Constant(_) => true
        Var(_,_) where expr in bound_vars => true
        Primitive(:reduce, _) => true
        _ => false
    end
end

# TODO where bound/remainder is |, need to push the abstract inside

function bound(bound_vars::Vector{Var}, var::Var, expr::Expr)::Expr
    bound(expr) = @match expr begin
        False() => expr
        True() => Var(:everything)
        Apply(f, args) => (var in args) ? permute(bound_vars, var, expr) : Var(:everything)
        Primitive(:|, [a, b]) => Primitive(:|, [bound(a), bound(b)])
        Primitive(:&, [a, b]) => Primitive(:&, [bound(a), bound(b)])
        Primitive(:(==), [a, b]) => begin
            if (a == var) && is_scalar_bound(bound_vars, b)
                b
            elseif (b == var) && is_scalar_bound(bound_vars, a)
                a
            else
                Var(:everything)
            end
        end
        Primitive(:exists, [arg]) => bound(arg)
        Primitive(:!, [arg]) => Var(:everything)
        Abstract(vars, value) => bound(value)
    end
    bound(expr)
end

function remainder(bound_vars::Vector{Var}, var::Var, expr::Expr)::Expr
    remainder(expr) = @match expr begin
        False() => expr
        True() => expr
        Apply(f, args) => issubset(args, union(bound_vars, (var, Var(:everything)))) ? True() : expr
        # TODO we can't simplify inside disjunction because bound covers both
        # Primitive(:|, [a, b]) => Primitive(:|, [remainder(a), remainder(b)])
        Primitive(:|, [a, b]) => Primitive(:|, [a, b])
        Primitive(:&, [a, b]) => Primitive(:&, [remainder(a), remainder(b)])
        Primitive(:(==), [a, b]) => begin
            if (a == var) && is_scalar_bound(bound_vars, b)
                True()
            elseif (b == var) && is_scalar_bound(bound_vars, a)
                True()
            else
                expr
            end
        end
        Primitive(:exists, [arg]) => Primitive(:exists, [remainder(arg)])
        Primitive(:!, [arg]) => expr
        Abstract(vars, value) => Abstract(vars, remainder(value))
    end
    remainder(expr)
end

function simplify_bound(expr::Expr)
    @match map_expr(simplify_bound, expr) begin
        Primitive(:|, [a && Var(:everything, 0), b]) => a
        Primitive(:|, [a, b && Var(:everything, 0)]) => b
        Primitive(:|, [a && False(), b]) => b
        Primitive(:|, [a, b && False()]) => a
        Primitive(:&, [a && Var(:everything, 0), b]) => b
        Primitive(:&, [a, b && Var(:everything, 0)]) => a
        Primitive(:&, [a && False(), b]) => a
        Primitive(:&, [a, b && False()]) => b
        other => other
    end
end

function simplify_remainder(expr::Expr)
    @match map_expr(simplify_remainder, expr) begin
        Primitive(:|, [a && True(), b]) => a
        Primitive(:|, [a, b && True()]) => b
        Primitive(:|, [a && False(), b]) => b
        Primitive(:|, [a, b && False()]) => a
        Primitive(:&, [a && True(), b]) => b
        Primitive(:&, [a, b && True()]) => a
        Primitive(:&, [a && False(), b]) => a
        Primitive(:&, [a, b && False()]) => b
        Primitive(:!, [False()]) => True()
        Primitive(:!, [True()]) => False()
        Primitive(:exists, [True()]) => True()
        Primitive(:exists, [False()]) => False()
        Primitive(:(==), [a, a]) => True()
        Primitive(:(==), [True(), False()]) => False()
        Primitive(:(==), [False(), True()]) => False()
        Primitive(:(==), [Constant(a), Constant(b)]) => False()
        Abstract(_, False()) => False()
        Abstract([], value) => value
        other => other
    end
end

function bound_abstract(bound_vars::Vector{Var}, expr::Abstract)::Expr
    # TODO need to also remove redundant computation from value
    value = expr.value
    bounds = Expr[]
    for i in 1:length(expr.vars)
        vars = vcat(bound_vars, expr.vars[1:i-1])
        var = expr.vars[i]
        push!(bounds, simplify_bound(bound(vars, var, value)))
        value = remainder(vars, var, value)
    end
    value = bound_abstract(vcat(expr.vars, bound_vars), simplify_remainder(value))
    for (var, bound) in zip(reverse(expr.vars), reverse(bounds))
        value = BoundedAbstract(var, bound, value)
    end
    value
end
bound_abstract(bound_vars::Vector{Var}, expr::Expr)::Expr = map_expr(expr -> bound_abstract(bound_vars, expr), expr)
bound_abstract(expr::Expr)::Expr = bound_abstract(Var[], expr)

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

const all_passes = [:parse, :lower, :bound_abstract, :interpret]

function imp(expr; globals=Dict{Symbol, Set}(), everything=nothing, passes=all_passes)
    env = Env{Set}(Var(name) => set for (name, set) in globals)
    if everything != nothing
        env[Var(:everything)] = everything
    end
    if :parse in passes
        expr = parse(expr)
        expr = separate_scopes(Scope(env), expr)
    end
    if :lower in passes
        expr = lower(env, expr)
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
