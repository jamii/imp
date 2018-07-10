const LoHi = UnitRange{Int64}

struct Factor{Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    column_ix::Int64
    lohi::LoHi
end

Factor(columns) = Factor(columns, 0, 1:length(columns[1]))

function factor_first(factor::Factor)::Tuple{Any, Factor}
    columns = tuple((column[factor.lohi] for column in factor.columns)...)
    column_ix = factor.column_ix + 1
    value = columns[column_ix][1]
    lohi = searchsorted(columns[column_ix], value)
    (value, Factor(columns, column_ix, lohi))
end

function factor_next(factor::Factor)::Union{Tuple{Any, Factor}, Nothing}
    factor.lohi.stop >= length(factor.columns[factor.column_ix]) && return nothing
    value = factor.columns[factor.column_ix][factor.lohi.stop + 1]
    lohi = searchsorted(factor.columns[factor.column_ix], value)
    (value, Factor(factor.columns, factor.column_ix, lohi))
end

function factor_seek(factor::Factor, value)::Union{Factor, Nothing}
    lohi = searchsorted(factor.columns[factor.column_ix], value)
    lohi.start > lohi.stop && return nothing
    Factor(factor.columns, factor.column_ix, lohi)
end

function factor_count(factor::Factor)::Int64
    factor.lohi.stop - factor.lohi.start + 1
end

struct GenericJoin
    factor_ixes
end

function foreach(f, join::GenericJoin, factors)
    factors = [factors...]
    (_, min_ix) = findmin([factor_count(factor) for factor in factors[[join.factor_ixes...]]])
    (value, min_factor) = factor_first(factors[join.factor_ixes[min_ix]])
    for (i, factor_ix) in enumerate(join.factor_ixes)
        if i != min_ix
            (_, factor) = factor_first(factors[factor_ix])
            factors[factor_ix] = factor
        end
    end
    while true
        for (i, factor_ix) in enumerate(join.factor_ixes)
            if i != min_ix
                factor = factor_seek(factors[factor_ix], value)
                factor == nothing && @goto next
                factors[factor_ix] = factor
            end
        end

        f(tuple(factors...))

        @label next
        next = @factor_next(min_factor)
        next == nothing && return
        (value, min_factor) = next
    end
end

struct Chain
    exprs::Tuple
end

function foreach(f, chain::Chain, factors)
    if isempty(chain.exprs)
        f(factors)
    else
        foreach(chain.exprs[1], factors) do factors
            foreach(f, Chain(chain.exprs[2:end]), factors)
        end
    end
end

function select(factor, ix)
    factor.columns[ix][factor.lohi.start]
end

function select(expr, factors, ixes)
    rows = []
    foreach(expr, factors) do factors
        row = tuple((select(factors[ix[1]], ix[2]) for ix in ixes)...)
        push!(rows, row)
    end
    rows
end

dep = Factor((["Eng", "Eng", "Sec"], ["Alice", "Bob", "Eve"]))
scale = Factor((["Eng", "Sec"], ["Hi", "Lo"]))

select(Chain(()), (dep, scale), ((1,1), (1,2), (2,2)))
select(Chain((GenericJoin((1,2)),)), (dep, scale), ((1,1), (1,2), (2,2)))
select(Chain((GenericJoin((1,2)), GenericJoin((1,)))), (dep, scale), ((1,1), (1,2), (2,2)))
