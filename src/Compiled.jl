module Compiled

using Base.Cartesian

function gallop{T}(column::AbstractArray{T}, lo::Int, hi::Int, value::T, threshold) 
  c = -1
  @inbounds if (lo < hi) && (c2 = cmp(column[lo], value); c *= c2; c2 < threshold)
    step = 1
    while (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  (lo, c) 
end 

mutable struct Range 
  lo::Int64
  hi::Int64
end

function narrow{T}(values::AbstractArray{T}, old_range::Range, new_range::Range, value::T)::Bool
  # start from end of last range
  new_range.lo, _ = gallop(values, new_range.hi, old_range.hi, value, 0)
  new_range.hi, _ = gallop(values, new_range.lo, old_range.hi, value, 1)
  new_range.lo < new_range.hi
end

function _join_init(n::Int64)
  join_init = Symbol("join_init_$n")
  column = [Symbol("column_$i") for i in 1:n] 
  old_range = [Symbol("old_range_$i") for i in 1:n] 
  new_range = [Symbol("new_range_$i") for i in 1:n] 
  
  quote 
    function $join_init($(column...), $(old_range...), $(new_range...))
      @nexprs $n (i) -> new_range_i.hi = old_range_i.lo
    end
  end
end

function _join_next_inner(n::Int64)
  join_next_inner = Symbol("join_next_inner_$n")
  column = [Symbol("column_$i") for i in 1:n] 
  old_range = [Symbol("old_range_$i") for i in 1:n]
  new_range = [Symbol("new_range_$i") for i in 1:n]
  
  quote 
    function $join_next_inner($(column...), $(old_range...), $(new_range...))
      # start from the end of the previous range
      new_range_1.lo = new_range_1.hi
      
      # bail if column_1 has no more values
      new_range_1.lo < old_range_1.hi || return false
      
      # figure out range of current value
      value = column_1[new_range_1.lo]
      new_range_1.hi, _ = gallop(column_1, new_range_1.lo, old_range_1.hi, value, 1)
    
      # check if other columns have a matching value
      return @nall $(n-1) (i) -> narrow(column_{i+1}, old_range_{i+1}, new_range_{i+1}, value)
    end
  end
end

function swapped(values, i) 
  values = copy(values)
  values[i], values[1] = values[1], values[i]
  values
end

function _join_next(n::Int64)
  join_next = Symbol("join_next_$n")
  join_next_inner = Symbol("join_next_inner_$n")
  column = [Symbol("column_$i") for i in 1:n]
  old_range = [Symbol("old_range_$i") for i in 1:n]
  new_range = [Symbol("new_range_$i") for i in 1:n]
  size = [:($(old_range[i]).hi - $(new_range[i]).hi) for i in 1:n]
    
  # last case, never called
  code = quote
    throw("Impossible!")
  end
  
  # check each size against min_size
  for i in 1:n
    code = quote
      if $(size[i]) == min_size
        $join_next_inner($(swapped(column, i)...), $(swapped(old_range, i)...), $(swapped(new_range, i)...))
      else 
        $code
      end
    end
  end
  
  quote 
    function $join_next($(column...), $(old_range...), $(new_range...))
      min_size = min($(size...))
      $code
    end
  end
end

macro join(f, args...)
  args = map(esc, args)
  n = Int64(length(args) / 3)
  @assert(f.head == :->)
  @assert(length(f.args[1].args) == 0)
  body = f.args[2]
  quote
    $(Symbol("join_init_$n"))($(args...))
    while true == $(Symbol("join_next_$n"))($(args...))
      $(esc(body))
    end
  end
end

for n in 1:10
  eval(_join_init(n))
  eval(_join_next_inner(n))
  eval(_join_next(n))
end

using BenchmarkTools

function polynomial(x_1, x_2, y_1, y_2)
  x_range_0 = Range(1, length(x_1) + 1)
  x_range_1 = Range(1, length(x_1) + 1)
  x_range_2 = Range(1, length(x_1) + 1)
  y_range_0 = Range(1, length(y_1) + 1)
  y_range_1 = Range(1, length(y_1) + 1)
  y_range_2 = Range(1, length(y_1) + 1)
  results_x = Int64[]
  results_y = Int64[]
  results_z = Int64[]
  # println(0, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
  @join(x_1, y_1, x_range_0, y_range_0, x_range_1, y_range_1) do
    # println(1, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
    @join(x_2, x_range_1, x_range_2) do
      # println(2, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
      @join(y_2, y_range_1, y_range_2) do
        # println(4, x_range_0, x_range_1, x_range_2, y_range_0, y_range_1, y_range_2)
        x = x_2[x_range_2.lo]
        y = y_2[y_range_2.lo]
        push!(results_x, x)
        push!(results_y, y)
        push!(results_z, (x * x) + (y * y) + (3 * x * y))
      end
    end
  end
  results_z
end

# @code_warntype polynomial([1,2],[1,4],[1,2],[1,-4])

@time polynomial(
collect(0:1000000), 
collect(0:1000000), 
collect(0:1000000), 
collect(reverse(0:1000000))
)

@show @benchmark polynomial(
$(collect(0:1000000)), 
$(collect(0:1000000)), 
$(collect(0:1000000)), 
$(collect(reverse(0:1000000)))
)
  
end
