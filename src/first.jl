# TODO check how missing is handled throughout. should probably just refuse to join on missing

struct Finger{Columns}
    columns::Columns
    ranges::Vector{UnitRange{Int64}}
end

finger(columns) = Finger(columns, [1:length(columns[1]) for i in 1:(length(columns)+1)])

function finger_first!(finger::Finger, ::Type{Val{column_ix}})::Nothing where column_ix
    bounds = finger.ranges[column_ix]
    finger.ranges[column_ix+1] = bounds.start:(bounds.start-1)
    nothing
end

function finger_next!(finger::Finger, ::Type{Val{column_ix}})::Any where column_ix
    column = finger.columns[column_ix]
    bounds = finger.ranges[column_ix]
    focus = finger.ranges[column_ix+1]
    focus.stop >= bounds.stop && return nothing
    start = focus.stop + 1
    value = column[start]
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    finger.ranges[column_ix+1] = start:stop
    value
end

function finger_seek!(finger::Finger, ::Type{Val{column_ix}}, value)::Bool where column_ix
    column = finger.columns[column_ix]
    bounds = finger.ranges[column_ix]
    focus = finger.ranges[column_ix+1]
    start = gallop(column, focus.stop + 1, bounds.stop + 1, value, 0)
    # TODO start + 1 ?
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    finger.ranges[column_ix+1] = start:stop
    start <= stop
end

function finger_count(finger::Finger, ::Type{Val{column_ix}})::Int64 where column_ix
    bounds = finger.ranges[column_ix]
    bounds.stop - bounds.start + 1
end

abstract type Query end

struct GenericJoin{ixes, Tail <: Query} <: Query
    tail::Tail
end
GenericJoin(ixes, tail) = GenericJoin{ixes, typeof(tail)}(tail)

@generated function execute(join::GenericJoin{ixes}, fingers) where {ixes}
    quote
        tail = join.tail

        $(@splice (finger_ix, column_ix) in ixes quote
          finger_first!(fingers[$finger_ix], Val{$column_ix})
          end)

        (_, min_ix) = findmin(tuple($(@splice (finger_ix, column_ix) in ixes quote
                                 finger_count(fingers[$finger_ix], Val{$column_ix})
                                 end)))
        min_ix = $ixes[min_ix][1]

        value = @match min_ix begin
            $(@splice (finger_ix, column_ix) in ixes :(
                $finger_ix => finger_next!(fingers[$finger_ix], Val{$column_ix})
            ))
        end

        while true
            $(@splice (finger_ix, column_ix) in ixes quote
              if $finger_ix != min_ix
              finger_seek!(fingers[$finger_ix], Val{$column_ix}, value) || @goto next
              end
              end)

            execute(tail, fingers)

            @label next
            next = @match min_ix begin
                $(@splice (finger_ix, column_ix) in ixes :(
                    $finger_ix => finger_next!(fingers[$finger_ix], Val{$column_ix})
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

function execute(product::Product{ix}, fingers) where ix
    (finger_ix, column_ix) = ix
    finger = fingers[finger_ix]
    tail = product.tail

    for i in finger.ranges[column_ix]
        finger.ranges[end] = i:i
        execute(tail, fingers)
    end
end

struct Select{ixes, Columns} <: Query
    columns::Columns
end
Select(ixes, columns) = Select{ixes, typeof(columns)}(columns)

@generated function execute(select::Select{ixes}, fingers) where {ixes}
    quote
        columns = select.columns
        $(@splice (i, (finger_ix, column_ix)) in enumerate(ixes) quote
          let
          local column = columns[$i]
          local finger = fingers[$finger_ix]
          local value = finger.columns[$column_ix][finger.ranges[end].start]
          push!(column, value)
          end
          end)
    end
end
