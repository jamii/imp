const RowType = NTuple{N, Type} where N
const SetType = Set{RowType}
const EnvTypes = Dict{Symbol, SetType}
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

function infer(env_types::EnvTypes, expr::Apply1)::SetType
    f_type = infer(env_types, expr.f)
    arg_type = infer(env_types, expr.arg)
    result_type = SetType()
    for f_row_type in f_type
        for arg_row_type in arg_type
            if length(arg_row_type) <= length(f_row_type) &&
                arg_row_type == f_row_type[1:length(arg_row_type)]
                push!(result_type, f_row_type[length(arg_row_type)+1:end])
            end
        end
    end
    result_type
end

function infer(env_types::EnvTypes, expr::Abstract1)::SetType
    result_type = SetType()
    env_types = copy(env_types)
    for variable_type in env_types[:everything]
        env_types[expr.var] = SetType([variable_type])
        for value_type in infer(env_types, expr.value)
            push!(result_type, (variable_type..., value_type...))
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
