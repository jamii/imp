const RowType = NTuple{N, Type} where N
const EnvTypes = Dict{Symbol, RowType}

function infer(env_types::EnvTypes, expr::Constant{T})::RowType where T
    (T,)
end

function infer(env_types::EnvTypes, expr::Constant{Bool})::RowType where T
    ()
end

function infer(env_types::EnvTypes, expr::Var)::RowType
    env_types[expr.name]
end

function infer(env_types::EnvTypes, expr::Apply)::RowType
    f_type = infer(env_types, expr.f)
    for arg_type in map((arg) -> infer(env_types, arg), expr.args)
        f_type = f_type[length(arg_type)+1:end]
    end
    f_type
end

function infer(env_types::EnvTypes, expr::Abstract)::RowType
    # TODO bound this better
    env_types = copy(env_types)
    env_types[expr.variable] = (Any,)
    value_type = infer(env_types, expr.value)
    (Any, value_type...)
end

function row_type_union(as::RowType, bs::RowType)
    @assert length(as) == length(bs)
    tuple((Union{a,b} for (a, b) in zip(as, bs))...)
end

function row_type_intersect(as::RowType, bs::RowType)
    @assert length(as) == length(bs)
    tuple((typeintersect(a,b) for (a, b) in zip(as, bs))...)
end

function infer(env_types::EnvTypes, expr::Primitive)::RowType
    arg_types = [infer(env_types, arg) for arg in expr.args]
    @match (expr.f, arg_types) begin
        (:|, _) => reduce(row_type_union, arg_types)
        (:&, _) => reduce(row_type_intersect, arg_types)
        (:!, [arg]) => ()
        (:(=>), [a, b]) => ()
        (:(==), _) => ()
        (:iff, [cond, true_branch]) => true_branch
        (:iff, [cond, true_branch, false_branch]) => row_type_union(true_branch, false_branch)
        (:reduce, [raw_op, raw_init, values]) => (raw_op[3],)
        (:exists, [arg]) => ()
        (:forall, [arg]) => ()
        _ => error("Unknown primitive: $expr")
    end
end
