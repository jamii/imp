const LoHi = UnitRange{Int64}

struct Factor{Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    lohi::LoHi
end

Factor(columns) = Factor(columns, 1:length(columns[1]))

function factor_first(factor::Factor)::Tuple{Any, Factor}
    value = factor.columns[1][1]
    lohi = searchsorted(factor.columns[1], value)
    (value, Factor(factor.columns, lohi))
end

function factor_next(factor::Factor)::Union{Tuple{Any, Factor}, Nothing}
    factor.lohi.stop >= length(factor.columns[1]) && return nothing
    value = factor.columns[1][factor.lohi.stop + 1]
    lohi = searchsorted(factor.columns[1], value)
    (value, Factor(factor.columns, lohi))
end

function factor_down(factor::Factor)::Factor
    @assert length(factor.columns) > 0
    columns = tuple((column[factor.lohi] for column in factor.columns[2:end])...)
    Factor(columns, 1:(factor.lohi.stop - factor.lohi.start + 1))
end

function foreach(f, factor::Factor)
    for i in factor.lohi
        row = tuple((column[i] for column in factor.columns)...)
        f(row)
    end
end

struct Product end

function foreach(f, product::Product, factors)
    if length(factors) == 0
        f(())
    else
        foreach(factors[1]) do row_left
            foreach(product, factors[2:end]) do row_right
                f(tuple(row_left..., row_right...))
            end
        end
    end
end        

struct MergeJoin{factor_ixes, Tail}
    tail::Tail
end

macro factor_next(f)
    quote
        n = factor_next($(esc(f)))
        n == nothing && return
        n
    end
end

function factors_merge(factors, (a, fa), (b, fb))
    factors = Any[factors...]
    factors[a] = fa
    factors[b] = fb
    tuple(factors...)
end

function foreach(f, join::MergeJoin{factor_ixes}, factors) where {factor_ixes}
    (a, b) = factor_ixes

    (va, fa) = factor_first(factors[a])
    (vb, fb) = factor_first(factors[b])

    while true
        c = cmp(va, vb)
        if c == 0
            foreach(join.tail, factors_merge(factors, a => factor_down(fa), b => factor_down(fb))) do row
                f((va, row...))
            end
            (va, fa) = @factor_next(fa)
            (vb, fb) = @factor_next(fb)
        elseif c == -1
            (va, fa) = @factor_next(fa)
        elseif c == 1
            (vb, fb) = @factor_next(fb)
        end
    end
end

function list(expr)
    rows = []
    foreach(expr) do row
        push!(rows, row)
    end
    rows
end

function list(expr, factors)
    rows = []
    foreach(expr, factors) do row
        push!(rows, row)
    end
    rows
end

dep = Factor((["Eng", "Eng", "Sec"], ["Alice", "Bob", "Eve"]))
scale = Factor((["Eng", "Sec"], ["Hi", "Lo"]))

list(dep)
list(Product(), (dep, scale))
list(MergeJoin{(1,2), Product}(Product()), (dep, scale))

