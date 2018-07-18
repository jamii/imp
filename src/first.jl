# TODO check how missing is handled throughout. should probably just refuse to join on missing

mutable struct Finger{column_ix, Columns, Default}
    columns::Columns
    range::UnitRange{Int64}
    default::Default
end

Finger(columns; default=nothing) = Finger{0, typeof(columns), typeof(default)}(columns, 1:length(columns[1]), default)

function finger_next_column(finger::Finger{column_ix}) where column_ix
    Finger{column_ix+1, typeof(finger.columns), typeof(finger.default)}(finger.columns, finger.range, finger.default)
end

function finger_last_column(finger::Finger{column_ix}) where column_ix
    Finger{length(finger.columns), typeof(finger.columns), typeof(finger.default)}(finger.columns, finger.range, finger.default)
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
    (finger.default !== nothing) || (start <= stop)
end

function finger_count(parent_finger::Finger, finger::Finger)::Int64
    bounds = parent_finger.range
    bounds.stop - bounds.start + 1
end

function finger_get(finger::Finger, ::Type{Val{column_ix}}) where column_ix
    focus = finger.range
    if focus.start <= focus.stop
        column = finger.columns[column_ix]
        column[focus.start]
    else
        @assert finger.default !== nothing
        @assert column_ix == length(finger.columns)
        finger.default
    end
end

struct GenericJoin{ixes, Tail}
    tail::Tail
end
GenericJoin(ixes, tail) = GenericJoin{ixes, typeof(tail)}(tail)

function start(join::GenericJoin{ixes}, in_fingers) where ixes
    out_fingers = Any[in_fingers...]
    for ix in ixes
        out_fingers[ix] = finger_next_column(out_fingers[ix])
    end
    (tuple(out_fingers...), start(join.tail, out_fingers))
end

@generated function execute(join::GenericJoin{ixes}, in_fingers, state) where ixes
    quote
        tail = join.tail
        (out_fingers, tail_state) = state

        $(@splice ix in ixes quote
          finger_first!(in_fingers[$ix], out_fingers[$ix])
          end)

        (_, min_ix) = findmin(tuple($(@splice ix in ixes quote
                                 finger_count(in_fingers[$ix], out_fingers[$ix])
                                 end)))
        min_ix = $ixes[min_ix]

        value = @match min_ix begin
            $(@splice ix in ixes :(
                $ix => finger_next!(in_fingers[$ix], out_fingers[$ix])
            ))
        end

        while true
            $(@splice ix in ixes quote
              if $ix != min_ix
              finger_seek!(in_fingers[$ix], out_fingers[$ix], value) || @goto next
              end
              end)

            execute(tail, out_fingers, tail_state)

            @label next
            next = @match min_ix begin
                $(@splice ix in ixes :(
                    $ix => finger_next!(in_fingers[$ix], out_fingers[$ix])
                ))
            end
            next == nothing && return
            value = next
        end
    end
end

function finish(join::GenericJoin, state)
    (out_fingers, tail_state) = state
    finish(join.tail, tail_state)
end

struct Product{ix, Tail}
    tail::Tail
end
Product(ix, tail) = Product{ix, typeof(tail)}(tail)

function start(product::Product{ix}, in_fingers) where ix
    out_fingers = Any[in_fingers...]
    out_fingers[ix] = finger_last_column(out_fingers[ix])
    (tuple(out_fingers...), start(product.tail, out_fingers))
end

function execute(product::Product{ix}, in_fingers, state) where ix
    tail = product.tail
    (out_fingers, tail_state) = state

    finger = out_fingers[ix]
    for i in in_fingers[ix].range
        out_fingers[ix].range = i:i
        execute(tail, out_fingers, tail_state)
    end
end

function finish(product::Product, state)
    (out_fingers, tail_state) = state
    finish(product.tail, tail_state)
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
          push!(state[$i], finger_get(fingers[$finger_ix], Val{$column_ix}))
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
