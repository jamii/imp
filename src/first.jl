const LoHi = UnitRange{Int64}

struct Factor{Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    column_ix::Int64
    lohi::LoHi
end

Factor(columns) = Factor(columns, 1, 1:length(columns[1]))

function factor_first(factor::Factor)::Tuple{Any, Factor}
    value = factor.columns[factor.column_ix][1]
    lohi = searchsorted(factor.columns[factor.column_ix], value)
    (value, Factor(factor.columns, factor.column_ix, lohi))
end

function factor_next(factor::Factor)::Union{Tuple{Any, Factor}, Nothing}
    factor.lohi.stop >= length(factor.columns[factor.column_ix]) && return nothing
    value = factor.columns[factor.column_ix][factor.lohi.stop + 1]
    lohi = searchsorted(factor.columns[factor.column_ix], value)
    (value, Factor(factor.columns, factor.column_ix, lohi))
end

function factor_down(factor::Factor)::Factor
    @assert !isempty(factor.columns)
    columns = tuple((column[factor.lohi] for column in factor.columns)...)
    column_ix = factor.column_ix + 1
    lohi = 1:(factor.lohi.stop - factor.lohi.start + 1)
    Factor(columns, column_ix, lohi)
end

macro factor_next(f)
    quote
        n = factor_next($(esc(f)))
        n == nothing && return
        n
    end
end

# function foreach(f, factor::Factor)
#     for i in factor.lohi
#         row = tuple((column[i] for column in factor.columns)...)
#         f(row)
#     end
# end

struct MergeJoin1{factor_ixes} end

MergeJoin1(factor_ixes) = MergeJoin1{factor_ixes}()

function factors_merge(factors, (a, fa))
    factors = Any[factors...]
    factors[a] = fa
    tuple(factors...)
end

function foreach(f, join::MergeJoin1{factor_ixes}, factors) where {factor_ixes}
    (a,) = factor_ixes

    (va, fa) = factor_first(factors[a])

    while true
        f(factors_merge(factors, a => factor_down(fa)))
        (va, fa) = @factor_next(fa)
    end
end

struct MergeJoin2{factor_ixes} end

MergeJoin2(factor_ixes) = MergeJoin2{factor_ixes}()

function factors_merge(factors, (a, fa), (b, fb))
    factors = Any[factors...]
    factors[a] = fa
    factors[b] = fb
    tuple(factors...)
end

function foreach(f, join::MergeJoin2{factor_ixes}, factors) where {factor_ixes}
    (a, b) = factor_ixes

    (va, fa) = factor_first(factors[a])
    (vb, fb) = factor_first(factors[b])

    while true
        c = cmp(va, vb)
        if c == 0
            f(factors_merge(factors, a => factor_down(fa), b => factor_down(fb)))
            (va, fa) = @factor_next(fa)
            (vb, fb) = @factor_next(fb)
        elseif c == -1
            (va, fa) = @factor_next(fa)
        elseif c == 1
            (vb, fb) = @factor_next(fb)
        end
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
select(Chain((MergeJoin2((1,2)),)), (dep, scale), ((1,1), (1,2), (2,2)))
select(Chain((MergeJoin2((1,2)), MergeJoin1((1,)))), (dep, scale), ((1,1), (1,2), (2,2)))

# struct Product end

# function foreach(f, product::Product, factors)
#     if length(factors) == 0
#         f(())
#     else
#         foreach(factors[1]) do row_left
#             foreach(product, factors[2:end]) do row_right
#                 f(tuple(row_left..., row_right...))
#             end
#         end
#     end
# end   
