# TODO check how missing is handled throughout. should probably just refuse to join on missing

struct Factor{Columns}
    columns::Columns
    ranges::Vector{UnitRange{Int64}}
end

Factor(columns) = Factor(columns, [1:length(columns[1]) for i in 1:(length(columns)+1)])

function factor_first!(factor::Factor, ::Type{Val{column_ix}})::Nothing where column_ix
    bounds = factor.ranges[column_ix]
    factor.ranges[column_ix+1] = bounds.start:(bounds.start-1)
    nothing
end

function factor_next!(factor::Factor, ::Type{Val{column_ix}})::Any where column_ix
    column = factor.columns[column_ix]
    bounds = factor.ranges[column_ix]
    focus = factor.ranges[column_ix+1]
    focus.stop >= bounds.stop && return nothing
    start = focus.stop + 1
    value = column[start]
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.ranges[column_ix+1] = start:stop
    value
end

function factor_seek!(factor::Factor, ::Type{Val{column_ix}}, value)::Bool where column_ix
    column = factor.columns[column_ix]
    bounds = factor.ranges[column_ix]
    focus = factor.ranges[column_ix+1]
    start = gallop(column, focus.stop + 1, bounds.stop + 1, value, 0)
    # TODO start + 1 ?
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    factor.ranges[column_ix+1] = start:stop
    start <= stop
end

function factor_count(factor::Factor, ::Type{Val{column_ix}})::Int64 where column_ix
    bounds = factor.ranges[column_ix]
    bounds.stop - bounds.start + 1
end

abstract type Query end

struct GenericJoin{ixes, Tail <: Query} <: Query
    tail::Tail
end
GenericJoin(ixes, tail) = GenericJoin{ixes, typeof(tail)}(tail)

@generated function execute(join::GenericJoin{ixes}, factors) where {ixes}
    quote
        tail = join.tail

        $(@splice (factor_ix, column_ix) in ixes quote
          factor_first!(factors[$factor_ix], Val{$column_ix})
          end)

        (_, min_ix) = findmin(tuple($(@splice (factor_ix, column_ix) in ixes quote
                                 factor_count(factors[$factor_ix], Val{$column_ix})
                                 end)))
        min_ix = $ixes[min_ix][1]

        value = @match min_ix begin
            $(@splice (factor_ix, column_ix) in ixes :(
                $factor_ix => factor_next!(factors[$factor_ix], Val{$column_ix})
            ))
        end

        while true
            $(@splice (factor_ix, column_ix) in ixes quote
              if $factor_ix != min_ix
              factor_seek!(factors[$factor_ix], Val{$column_ix}, value) || @goto next
              end
              end)

            execute(tail, factors)

            @label next
            next = @match min_ix begin
                $(@splice (factor_ix, column_ix) in ixes :(
                    $factor_ix => factor_next!(factors[$factor_ix], Val{$column_ix})
                ))
            end
            next == nothing && return
            value = next
        end
    end
end

struct Product{ix, Tail <: Query} <: Query
    tail::Tail
end
Product(ix, tail) = Product{ix, typeof(tail)}(tail)

function execute(product::Product{ix}, factors) where ix
    (factor_ix, column_ix) = ix
    factor = factors[factor_ix]
    tail = product.tail

    for i in factor.ranges[column_ix]
        factor.ranges[end] = i:i
        execute(tail, factors)
    end
end

struct Select{ixes, Columns} <: Query
    columns::Columns
end
Select(ixes, columns) = Select{ixes, typeof(columns)}(columns)

@generated function execute(select::Select{ixes}, factors) where {ixes}
    quote
        columns = select.columns
        $(@splice (i, (factor_ix, column_ix)) in enumerate(ixes) quote
          let
          local column = columns[$i]
          local factor = factors[$factor_ix]
          local value = factor.columns[$column_ix][factor.ranges[end].start]
          push!(column, value)
          end
          end)
    end
end
