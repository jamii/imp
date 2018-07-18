# TODO check how missing is handled throughout. should probably just refuse to join on missing

mutable struct Finger{column_ix, Columns}
    columns::Columns
    range::UnitRange{Int64}
end

Finger(columns) = Finger(columns, [1:length(columns[1]) for i in 1:(length(columns)+1)])

function finger_next_column(finger::Finger{column_ix}) where column_ix
    Finger{column_ix+1, typeof(finger.columns)}(finger.columns, finger.range)
end

function finger_first!(parent_finger::Finger, finger::Finger)::Nothing where column_ix
    bounds = parent_finger.range
    finger.range = bounds.start:(bounds.start-1)
    nothing
end

function finger_next!(parent_finger::Finger, finger::Finger{column_ix})::Any where column_ix
    column = finger.columns[column_ix]
    bounds = parent_finger.range
    focus = finger.range
    focus.stop >= bounds.stop && return nothing
    start = focus.stop + 1
    value = column[start]
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    finger.range = start:stop
    value
end

function finger_seek!(parent_finger::Finger, finger::Finger{column_ix}, value)::Bool where column_ix
    column = finger.columns[column_ix]
    bounds = parent_finger.range
    focus = finger.range
    start = gallop(column, focus.stop + 1, bounds.stop + 1, value, 0)
    # TODO start + 1, get(stop) == value?
    stop = gallop(column, start, bounds.stop + 1, value, 1) - 1
    finger.range = start:stop
    start <= stop
end

function finger_count(parent_finger::Finger, finger::Finger)::Int64 where column_ix
    bounds = parent_finger.range
    bounds.stop - bounds.start + 1
end

# defaults
start(query, fingers) = start(query.tail, fingers)
execute(query, fingers, state) = execute(query.tail, fingers, state)
finish(query, state) = finish(query.tail, state)

struct GenericJoin{ixes, Tail}
    tail::Tail
end
GenericJoin(ixes, tail) = GenericJoin{ixes, typeof(tail)}(tail)

function start(join::GenericJoin{ixes}, in_fingers)
    out_fingers = Any[in_fingers...]
    for (finger_ix, _) in ixes

@generated function execute(join::GenericJoin{ixes}, fingers, state) where {ixes}
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

            execute(tail, fingers, state)

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

struct Product{ix, Tail}
    tail::Tail
end
Product(ix, tail) = Product{ix, typeof(tail)}(tail)

function execute(product::Product{ix}, fingers, state) where ix
    (finger_ix, column_ix) = ix
    finger = fingers[finger_ix]
    tail = product.tail

    for i in finger.ranges[column_ix]
        finger.ranges[end] = i:i
        execute(tail, fingers, state)
    end
end

struct Select{ixes} end
Select(ixes) = Select{ixes}()

@generated function start(select::Select{ixes}, fingers) where {ixes}
    quote
        tuple($(@splice (finger_ix, column_ix) in ixes quote
                empty(fingers[$finger_ix].columns[$column_ix])
                end))
    end
end

@generated function execute(select::Select{ixes}, fingers, state) where {ixes}
    quote
        $(@splice (i, (finger_ix, column_ix)) in enumerate(ixes) quote
          let
          local column = state[$i]
          local finger = fingers[$finger_ix]
          local value = finger.columns[$column_ix][finger.ranges[end].start]
          push!(column, value)
          end
          end)
    end
end

finish(select::Select, state) = state

struct Count end
start(count::Count, fingers) = Ref(0)
execute(count::Count, fingers, state) = state[] += 1
finish(count::Count, state) = state[]

function run(query, fingers)
    state = start(query, fingers)
    execute(query, fingers, state)
    finish(query, state)
end
