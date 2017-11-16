module Compiled

using Base.Cartesian

function gallop{T}(column::AbstractArray{T}, lo::UInt64, hi::UInt64, value::T, threshold::Int64)::Tuple{UInt64, Int64}
  c = -1
  @inbounds if (lo < hi) && (c2 = cmp(column[lo], value); c *= c2; c2 < threshold)
    step = UInt64(1)
    while (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
      lo = lo + step
      step = step << UInt64(1)
    end

    step = step >> UInt64(1)
    while step > UInt64(1)
      if (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
        lo = lo + step
      end
      step = step >> UInt64(1)
    end

    lo += UInt64(1)
  end
  (lo, c)
end

# why inexacterror?
@code_llvm gallop([1,2,3], UInt64(0), UInt64(1), 2, 1)

mutable struct Range
  lo::UInt64
  hi::UInt64
end

@inline function narrow{T}(values::AbstractArray{T}, old_range::Range, new_range::Range, value::T)::Bool
  # start from end of last range
  new_range.lo, c = gallop(values, new_range.hi, old_range.hi, value, 0)
  if c == 0
    new_range.hi, _ = gallop(values, new_range.lo+1, old_range.hi, value, 1)
    new_range.lo < new_range.hi
  else
    new_range.hi = new_range.lo
    false
  end
end

abstract type AbstractJoin end

function _Join(n::Int64)
  Join = Symbol("Join_$n")
  column = [:($(Symbol("column_$i"))::T) for i in 1:n]
  old_range = [:($(Symbol("old_range_$i"))::Range) for i in 1:n]
  new_range = [:($(Symbol("new_range_$i"))::Range) for i in 1:n]
  quote
    mutable struct $Join{T} <: AbstractJoin
      $(column...)
      $(old_range...)
      $(new_range...)
    end
  end
end

const _Joins = Dict{Int64, Type}()

function Join(n::Int64)
  if !haskey(_Joins, n)
    eval(_Join(n))
    _Joins[n] = eval(Symbol("Join_$n"))
  end
  _Joins[n]
end

function _join_init(n::Int64)
  quote
    $(Expr(:meta, :inline))

    # calculate size of each range
    @nexprs $n (i) -> size_i = join.old_range_i.hi - join.new_range_i.hi

    # pick smallest size
    min_size = @ncall $n min (i) -> size_i

    # swap smallest range to 1st
    @nif $n (i) -> size_i == min_size (i) -> begin
      join.column_i, join.column_1 = join.column_1, join.column_i
      join.old_range_i, join.old_range_1 = join.old_range_1, join.old_range_i
      join.new_range_i, join.new_range_1 = join.new_range_1, join.new_range_i
    end

    # init new_ranges
    @nexprs $n (i) -> join.new_range_i.hi = join.old_range_i.lo
  end
end

@generated function join_init(join::AbstractJoin)
  _join_init(Int64(length(fieldnames(join)) / 3))
end

function _join_next(n::Int64)
  quote
    $(Expr(:meta, :inline))

    while true
      # start from the end of the previous range
      join.new_range_1.lo = join.new_range_1.hi
      
      # bail if column_1 is out of values
      if join.new_range_1.lo >= join.old_range_1.hi
        return false
      end
      
      # figure out range of current value
      @inbounds value = join.column_1[join.new_range_1.lo]
      join.new_range_1.hi, _ = gallop(join.column_1, join.new_range_1.lo, join.old_range_1.hi, value, 1)

      # check if other columns have a matching value
      if @nall $n (i) -> (i == 1 || narrow(join.column_i, join.old_range_i, join.new_range_i, value))
        return true
      end
    end
  end
end

@generated function join_next(join::AbstractJoin)
  _join_next(Int64(length(fieldnames(join)) / 3))
end

macro join(f, join)
  @assert(f.head == :->)
  @assert(length(f.args[1].args) == 0)
  body = f.args[2]
  quote
    const join = $(esc(join))
    join_init(join)
    while true == join_next(join)
      $(esc(body))
    end
  end
end

using BenchmarkTools

Join(1)
Join(2)

function polynomial(x_1, x_2, y_1, y_2)
  const x_range_0 = Range(1, length(x_1) + 1)
  const x_range_1 = Range(1, length(x_1) + 1)
  const x_range_2 = Range(1, length(x_1) + 1)
  const y_range_0 = Range(1, length(y_1) + 1)
  const y_range_1 = Range(1, length(y_1) + 1)
  const y_range_2 = Range(1, length(y_1) + 1)
  const results_x = Int64[]
  const results_y = Int64[]
  const results_z = Int64[]
  const j1 = Join_2(x_1, y_1, x_range_0, y_range_0, x_range_1, y_range_1)
  const j2 = Join_1(x_2, x_range_1, x_range_2)
  const j3 = Join_1(y_2, y_range_1, y_range_2)
  @join(j1) do
    # println(1, x_range_1, y_range_1)
    @join(j2) do
      # println(2, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
      @join(j3) do
        # println(4, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
        @inbounds x = x_2[x_range_2.lo]
        @inbounds y = y_2[y_range_2.lo]
        push!(results_x, x)
        push!(results_y, y)
        push!(results_z, (x * x) + (y * y) + (3 * x * y))
      end
    end
  end
  results_z
end

# @code_warntype polynomial([1,2],[1,4],[1,2],[1,-4])
# @code_llvm polynomial([1,2],[1,4],[1,2],[1,-4])

@show @time polynomial(
collect(0:10),
collect(0:10),
collect(0:10),
collect(reverse(0:10))
)

# @time polynomial(
# collect(0:1000000),
# collect(0:1000000),
# collect(0:1000000),
# collect(reverse(0:1000000))
# )

@show @benchmark polynomial(
$(collect(0:1000000)),
$(collect(0:1000000)),
$(collect(0:1000000)),
$(collect(reverse(0:1000000)))
)

end
