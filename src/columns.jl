TupleOfVectors = NTuple{N, Vector{T} where T} where N

# --- sorting ---
# TODO clean this section up

@generated function cmp_in(xs::T, ys::T, x_at::Int, y_at::Int)::Int64 where {T <: TupleOfVectors}
    n = length(T.parameters)
    if n == 0
        :(return 0)
    else 
        quote
            $(Base.Expr(:meta, :inline))
            @inbounds begin 
                $([:(result = cmp(xs[$c][x_at], ys[$c][y_at]); if result != 0; return result; end) for c in 1:(n-1)]...)
                return cmp(xs[$n][x_at], ys[$n][y_at])
            end
        end
    end
end

@generated function swap_in!(xs::T, i::Int, j::Int) where {T <: TupleOfVectors}
    n = length(T.parameters)
    quote
        $(Base.Expr(:meta, :inline))
        @inbounds begin 
            $([quote 
               let tmp = xs[$c][i]
               xs[$c][i] = xs[$c][j]
               xs[$c][j] = tmp
               end	    
               end for c in 1:n]...)
        end
    end
end

@generated function set_in!(xs::T, i::Int, j::Int) where {T <: TupleOfVectors}
    n = length(T.parameters)
    quote
        $(Base.Expr(:meta, :inline))
        @inbounds begin 
            $([quote 
               xs[$c][i] = xs[$c][j]
               end for c in 1:n]...)
        end
    end
end

@generated function push_in!(result::T, cs::T, i::Int) where {T <: TupleOfVectors}
    n = length(T.parameters)
    quote
        $(Base.Expr(:meta, :inline))
        @inbounds begin 
            $([:(push!(result[$c], cs[$c][i])) for c in 1:n]...)
        end
    end
end

@generated function push_in!(result::T, row) where {T <: TupleOfVectors}
    n = length(T.parameters)
    quote
        $(Base.Expr(:meta, :inline))
        @inbounds begin 
            $([:(push!(result[$c], row[$c])) for c in 1:n]...)
        end
    end
end

# sorting cribbed from Base.Sort
# but unrolls `cmp` and `swap` to avoid heap allocation of rows
# and use random pivot selection because stdlib pivot caused 1000x sadness on real data

function insertion_sort!(cs::T, lo::Int, hi::Int) where {T <: TupleOfVectors}
    @inbounds for i = lo+1:hi
        j = i
        while j > lo && (cmp_in(cs, cs, j, j-1) == -1)
            swap_in!(cs, j, j-1)
            j -= 1
        end
    end
end

function partition!(cs::T, lo::Int, hi::Int) where {T <: TupleOfVectors}
    @inbounds begin
        pivot = rand(lo:hi)
        swap_in!(cs, pivot, lo)
        i, j = lo+1, hi
        while true
            while (i <= j) && (cmp_in(cs, cs, i, lo) == -1); i += 1; end;
            while (i <= j) && (cmp_in(cs, cs, lo, j) == -1); j -= 1; end;
            i >= j && break
            swap_in!(cs, i, j)
            i += 1; j -= 1
        end
        swap_in!(cs, lo, j)
        return j
    end
end

function quicksort!(cs::T, lo::Int, hi::Int) where {T <: TupleOfVectors}
    @inbounds if hi-lo <= 0
        return
    elseif hi-lo <= 20 
        insertion_sort!(cs, lo, hi)
    else
        j = partition!(cs, lo, hi)
        quicksort!(cs, lo, j-1)
        quicksort!(cs, j+1, hi)
    end
end

function quicksort!(cs::T) where {T <: TupleOfVectors}
    quicksort!(cs, 1, length(cs[1]))
end

function is_sorted(data::T)::Bool where {T <: TupleOfVectors}
    for i in 2:length(data[1])
        if cmp_in(data, data, i, i-1) == -1
            return false
        end
    end
    return true
end

@generated function dedup_sorted!(data::T) where {T <: TupleOfVectors}
    quote 
        read = 1
        write = 1
        for outer read in 2:length(data[1])
            if cmp_in(data, data, read, write) != 0
                write += 1
                set_in!(data, write, read)
            end
        end
        $(@splice c in 1:length(T.parameters) :(resize!(data[$c], write)))
    end
end

function diff!(a::T, b::T, f_a, f_b, f_ab) where {T <: TupleOfVectors}
  @inbounds begin
    at_a = 1
    at_b = 1
    hi_a = length(a[1])
    hi_b = length(b[1])
    while at_a <= hi_a && at_b <= hi_b
      c = cmp_in(a, b, at_a, at_b)
      if c == 0
        f_ab(a, b, at_a, at_b)
        at_a += 1
        at_b += 1
      elseif c == 1
        f_b(b, at_b)
        at_b += 1
      else 
        f_a(a, at_a)
        at_a += 1
      end
    end
    while at_a <= hi_a
      f_a(a, at_a)
      at_a += 1
    end
    while at_b <= hi_b
      f_b(b, at_b)
      at_b += 1
    end
  end
end

# --- columns ---

@generated num_columns(::Type{Row}) where Row <: Tuple = length(Row.parameters)
@generated columns_elem_type(::Type{Vector{T}}) where T = T
@generated columns_data_type(::Type{Row}) where Row <: Tuple = Tuple{map((p) -> Vector{p}, Row.parameters)...}
@generated columns_row_type(::Type{Data}) where Data <: TupleOfVectors = Tuple{map(columns_elem_type, Data.parameters)...}

@generated function check_lengths(data::T) where T <: TupleOfVectors
    quote
        $(@splice c in 1:num_columns(T) :(@assert(length(data[$c]) == length(data[1]))))
    end
end

struct Columns{Row <: Tuple, Data <: TupleOfVectors} <: AbstractSet{Row}
    data::Data
    count::Int64 # we need this because we can't tell the different between Columns([]) and Columns([()])
    
    function Columns{Row, Data}(data::Data, is_empty::Bool) where {Row <: Tuple, Data <: TupleOfVectors}
        @assert columns_row_type(Data) == Row
        if length(data) > 0
            check_lengths(data)
            !is_sorted(data) && quicksort!(data)
            dedup_sorted!(data)
            new(data, length(data[1]))
        else
            new(data, is_empty ? 0 : 1)
        end
    end
end

function Columns(data::Data, is_empty::Bool) where {Data <: TupleOfVectors}
    Columns{columns_row_type(Data), Data}(data, is_empty)
end

# TODO default show should work, but io in 0.7 is totally broken atm
function Base.show(io::IO, columns::Columns{Row}) where {Row}
    print(io, "Imp.Columns{$Row}([")
    for row in columns
        print(io, row, ", ")
    end
    print(io, "])")
end
Base.display(columns::Columns) = show(columns)

@generated function permute(columns::Columns{Row}, ::Type{Val{ixes}}) where {Row, ixes}
    quote
        @assert ixes isa (NTuple{N,Int64} where N)
        data = tuple($(@splice i in 1:length(ixes) :(copy(columns.data[$(ixes[i])]))))
        Columns(data, columns.count == 0)
    end
end

# --- set interface ---
# mostly unoptimized - will allocate heavily if used with !isbits types

@generated function empty_data(::Type{Data}) where {Data <: TupleOfVectors}
    quote
        tuple($(@splice c in 1:num_columns(Data) :($(Data.parameters[c])())))
    end
end

function Columns(iter)
    Row = eltype(iter)
    if Row <: Tuple
        Columns{Row}(iter)
    else
        # TODO probably need to deal with this case for the naive interpreter
        error("Bad row type: $Row")
    end
end
Columns{Row}(iter) where {Row} = Columns{Row, columns_data_type(Row)}(iter)

function Columns{Row, Data}(iter) where {Row, Data}
    data = empty_data(Data)
    is_empty = true
    state = start(iter)
    while !done(iter, state)
        (row, state) = next(iter, state)
        push_in!(data, row)
        is_empty = false
    end
    Columns{Row, Data}(data, is_empty)
end

Base.eltype(::Type{Columns{Row}}) where {Row} = Row
Base.length(columns::Columns) = columns.count

Base.start(columns::Columns) = 1
Base.done(columns::Columns, r::Int64) = r > length(columns)
Base.next(columns::Columns, r::Int64) = (columns[r], r+1)

@generated function Base.getindex(columns::Columns{Row}, r::Int64)::Row where {Row}
    quote
        $(Base.Expr(:meta, :inline))
        $(Base.Expr(:meta, :propagate_inbounds))
        @boundscheck if !(1 <= r <= columns.count)
            throw(BoundsError(columns, r))
        end
        tuple($(@splice c in 1:num_columns(Row) :(columns.data[$c][r])))
    end
end

function Base.in(row::Row, columns::Columns{Row}) where {Row}
    searchsortedfirst(columns, row) <= length(columns)
end

# don't fall back to linear search
Base.in(row::Row1, columns::Columns{Row2}) where {Row1, Row2} = error("$Row1 in Columns{$Row2}")

# == falls back to length and issubset
function Base.issubset(columns1::Columns{Row}, columns2::Columns{Row}) where {Row}
    result = true
    diff!(columns1.data, columns2.data,
          (a, a_at) -> result = false,
          (b, b_at) -> (),
          (a, b, a_at, b_at) -> ())
    result
end

function Base.intersect(columns1::Columns{Row, Data}, columns2::Columns{Row, Data}) where {Row, Data}
    data = empty_data(Data)
    diff!(columns1.data, columns2.data,
          (a, a_at) -> nothing,
          (b, b_at) -> nothing,
          (a, b, a_at, b_at) -> push_in!(data, a, a_at))
    Columns(data, isempty(columns1) || isempty(columns2))
end

function Base.union(columns1::Columns{Row, Data}, columns2::Columns{Row, Data}) where {Row, Data}
    data = empty_data(Data)
    diff!(columns1.data, columns2.data,
          (a, at_a) -> push_in!(data, a, at_a),
          (b, at_b) -> push_in!(data, b, at_b),
          (a, b, at_a, at_b) -> push_in!(data, a, at_a))
    Columns(data, isempty(columns1) && isempty(columns2))
end
    
function Base.setdiff(columns1::Columns{Row, Data}, columns2::Columns{Row, Data}) where {Row, Data}
    data = empty_data(Data)
    is_empty = true
    diff!(columns1.data, columns2.data,
          (a, a_at) -> (is_empty = false; push_in!(data, a, a_at)),
          (b, b_at) -> nothing,
          (a, b, a_at, b_at) -> nothing)
    Columns(data, is_empty)
end

# --- tmp ---

function gallop(column::AbstractArray, lo::Int64, hi::Int64, value, f) ::Int64
    if (lo < hi) && f(column[lo], value)
        step = 1
        while (lo + step < hi) && f(column[lo + step], value)
            lo = lo + step
            step = step << 1
        end

        step = step >> 1
        while step > 0
            if (lo + step < hi) && f(column[lo + step], value)
                lo = lo + step
            end
            step = step >> 1
        end

        lo += 1
    end
    lo
end

export Columns
