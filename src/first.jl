# TODO check how missing is handled throughout. should probably just refuse to join on missing

mutable struct Factor{ColumnIx, Columns <: NTuple{N, Vector} where N}
    columns::Columns # at least one column, at least one row
    bounds::UnitRange{Int64}
    focus::UnitRange{Int64}
end

Factor(columns) = Factor{0, typeof(columns)}(columns, 1:length(columns[1]), 1:length(columns[1]))

function factor_next_column(in_factor::Factor{column_ix})::Factor where {column_ix}
    Factor{column_ix+1, typeof(in_factor.columns)}(in_factor.columns, 1:0, 1:0)
end

function factor_first!(in_factor::Factor, out_factor::Factor)
    out_factor.bounds = in_factor.focus
    out_factor.focus = in_factor.focus.start:(in_factor.focus.start-1)
end

function factor_next!(factor::Factor{column_ix})::Union{Any, Nothing} where {column_ix}
    column = factor.columns[column_ix]
    focus = factor.focus
    bounds = factor.bounds
    focus.stop >= bounds.stop && return nothing
    start = focus.stop + 1
    value = column[start]
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.focus = start:stop
    value
end

function factor_seek!(factor::Factor{column_ix}, value)::Bool where {column_ix}
    column = factor.columns[column_ix]
    focus = factor.focus
    bounds = factor.bounds
    start = gallop(column, focus.stop + 1, bounds.stop + 1, value, 0)
    # TODO start + 1 ?
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.focus = start:stop
    start <= stop
end

function factor_count(factor::Factor)::Int64
    factor.bounds.stop - factor.bounds.start + 1
end

abstract type Query end

struct GenericJoin <: Query
    factor_ixes
    tail::Query
end

struct StagedGenericJoin{FactorIxes, InFactors, OutFactors, Tail}
    in_factors::InFactors
    out_factors::OutFactors
    tail::Tail
end

function stage(join::GenericJoin, in_factors)
    out_factors = Any[in_factors...]
    for factor_ix in join.factor_ixes
        out_factors[factor_ix] = factor_next_column(in_factors[factor_ix])
    end
    out_factors = tuple(out_factors...)
    tail = stage(join.tail, out_factors)
    StagedGenericJoin{join.factor_ixes, typeof(in_factors), typeof(out_factors), typeof(tail)}(in_factors, out_factors, tail)
end

@generated function execute(join::StagedGenericJoin{factor_ixes}) where {factor_ixes}
    quote
        in_factors = join.in_factors
        out_factors = join.out_factors
        tail = join.tail

        $(@splice factor_ix in factor_ixes quote
          factor_first!(in_factors[$factor_ix], out_factors[$factor_ix])
          end)

        (_, min_ix) = findmin(tuple($(@splice factor_ix in factor_ixes quote
                                 factor_count(out_factors[$factor_ix])
                                 end)))
        min_ix = $factor_ixes[min_ix]

        value = @match min_ix begin
            $(@splice factor_ix in [1,2,3] :(
                $factor_ix => factor_next!(out_factors[$factor_ix])
            ))
        end

        while true
            $(@splice factor_ix in factor_ixes quote
              if $factor_ix != min_ix
              factor_seek!(out_factors[$factor_ix], value) || @goto next
              end
              end)

            execute(tail)

            @label next
            next = @match min_ix begin
                $(@splice factor_ix in [1,2,3] :(
                    $factor_ix => factor_next!(out_factors[$factor_ix])
                ))
            end
            next == nothing && return
            value = next
        end
    end
end

function output(join::StagedGenericJoin)
    output(join.tail)
end

struct Product <: Query
    factor_ix
    tail::Query
end

struct StagedProduct{FactorIx, InFactors, OutFactors, Tail}
    in_factors::InFactors
    out_factors::OutFactors
    tail::Tail
end

function stage(product::Product, in_factors)
    out_factors = Any[in_factors...]
    out_factors[product.factor_ix] = factor_next_column(out_factors[product.factor_ix])
    out_factors = tuple(out_factors...)
    tail = stage(product.tail, out_factors)
    StagedProduct{product.factor_ix, typeof(in_factors), typeof(out_factors), typeof(tail)}(in_factors, out_factors, tail)
end

function execute(product::StagedProduct{Ix}) where {Ix}
    in_factor = product.in_factors[Ix]
    out_factor = product.out_factors[Ix]
    tail = product.tail

    factor_first!(in_factor, out_factor)

    for i in out_factor.bounds
        out_factor.focus = i:i
        execute(tail)
    end
end

function output(product::StagedProduct)
    output(product.tail)
end

struct Select <: Query
    ixes
end

struct StagedSelect{Ixes, InFactors, Columns}
    in_factors::InFactors
    columns::Columns
end

function stage(select::Select, in_factors)
    ixes = select.ixes
    columns = tuple((Vector{eltype(in_factors[ix[1]].columns[ix[2]])}() for ix in ixes)...)
    StagedSelect{ixes, typeof(in_factors), typeof(columns)}(in_factors, columns)
end

@generated function execute(select::StagedSelect{ixes}) where {ixes}
    quote
        in_factors = select.in_factors
        columns = select.columns
        $(@splice (i, ix) in enumerate(ixes) quote
          let
          local column = columns[$i]
          local factor = in_factors[$ix[1]]
          local value = factor.columns[$ix[2]][factor.focus.start]
          push!(column, value)
          end
          end)
    end
end

function output(select::StagedSelect)
    select.columns
end

function run(expr::Query, factors)
    staged = stage(expr, factors)
    execute(staged)
    output(staged)
end
