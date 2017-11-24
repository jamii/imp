module Compiled

using Base.Cartesian

# interface for all functions

function is_function(fun_type)
  false
end 
# function call(fun, args...) end 
# function permute(fun, columns) end 

# interface for finite functions

function is_finite(fun_type) 
  false
end 
# function finger(fun) end 
# function count(fun, finger) end 
# function first(fun, outer_finger) end
# function next(fun, outer_finger, inner_finger) end
# function seek(fun, finger, value) end
# function get(fun, finger) end

# implementation for tuple of column vectors

struct Relation{T <: Tuple} 
  columns::T
end

struct RelationFinger{Column}
  lo::UInt64 # inclusive
  hi::UInt64 # exclusive
end

function gallop{T}(column::AbstractArray{T}, lo::UInt64, hi::UInt64, value::T, threshold::Int64)::UInt64
  @inbounds if (lo < hi) && cmp(column[lo], value) < threshold
    step = UInt64(1)
    while (lo + step < hi) && cmp(column[lo + step], value) < threshold
      lo = lo + step
      step = step << UInt64(1)
    end

    step = step >> UInt64(1)
    while step > UInt64(1)
      if (lo + step < hi) && cmp(column[lo + step], value) < threshold
        lo = lo + step
      end
      step = step >> UInt64(1)
    end

    lo += UInt64(1)
  end
  lo
end

function is_finite(::Type{Relation}) 
  true
end
  
function finger(fun::Relation)
  RelationFinger{0}(1, length(fun.columns[1]) + 1)
end

function count(fun::Relation, finger::RelationFinger) 
  # not actually correct, but will do for now
  finger.hi - finger.lo
end

@generated function first(fun::Relation, outer_finger::RelationFinger{C}) where {C}
  C2 = C + 1
  quote
    column = fun.columns[$C2]
    lo = outer_finger.lo
    value = column[lo]
    hi = gallop(column, lo, outer_finger.hi, value, 1)
    RelationFinger{$C2}(lo, hi)
  end
end

@generated function next(fun::Relation, outer_finger::RelationFinger{C}, inner_finger::RelationFinger{C2}) where {C, C2}
  @assert C2 == C + 1
  quote
    column = fun.columns[C2]
    lo = inner_finger.hi
    if lo < outer_finger.hi
      value = column[lo]
      hi = gallop(column, lo, outer_finger.hi, value, 1)
      RelationFinger{C2}(lo, hi)
    else 
      RelationFinger{C2}(lo, outer_finger.hi)
    end
  end
end

# TODO can push this into gallop, to search lo and hi at same time
#      or maybe use searchsorted if we can remove the cost of the subarray
@generated function seek(fun::Relation, finger::RelationFinger{C}, value) where {C}
  C2 = C + 1
  quote 
    column = fun.columns[$C2]
    lo = gallop(column, finger.lo, finger.hi, value, 0)
    hi = gallop(column, lo+1, finger.hi, value, 1)
    RelationFinger{$C2}(lo, hi)
  end
end

function get(fun::Relation, finger::RelationFinger{C}) where {C}
  fun.columns[C][finger.lo]
end

function _join(num_funs, ixes, num_results, f)
  n = length(ixes)
  funs = [Symbol("ignored_fun_$i") for i in 1:num_funs]
  outer_fingers = [Symbol("ignored_outer_finger_$i") for i in 1:num_funs]
  result_fingers = [Symbol("ignored_outer_finger_$i") for i in 1:num_funs]
  for i in 1:length(ixes)
    funs[ixes[i]] = Symbol("fun_$i")
    outer_fingers[ixes[i]] = Symbol("outer_finger_$i")
    result_fingers[ixes[i]] = Symbol("inner_finger_$i")
  end
  inner_fingers = [Symbol("inner_finger_$i") for i in 1:length(ixes)]
  results = [Symbol("result_$i") for i in 1:num_results]
  quote 
    function($(funs...), $(outer_fingers...), $(results...))
      @nexprs $n (i) -> count_i = count(fun_i, outer_finger_i)
      min_count = @ncall $n min (i) -> count_i
      @nif $n (min) -> count_min == min_count (min) -> begin
        inner_finger_min = first(fun_min, outer_finger_min)
        while count(fun_min, inner_finger_min) > 0
          let value = get(fun_min, inner_finger_min)
            if (@nall $n (i) -> ((i == min) || begin
                inner_finger_i = seek(fun_i, outer_finger_i, value)
                count(fun_i, inner_finger_i) > 0
              end))
              $f($(funs...), $(result_fingers...), $(results...))
            end
            inner_finger_min = next(fun_min, outer_finger_min, inner_finger_min)
          end
        end
      end
    end
  end
end

macroexpand(_join(2, [1,2], 0, :f))

function join(num_funs, ixes, num_results, f)
  eval(_join(num_funs, ixes, num_results, f))
end

using BenchmarkTools

function make_polynomial()
  j0(xx, yy, xx_finger, yy_finger, results_x, results_y, results_z) = begin
    x = get(xx, xx_finger)
    y = get(yy, yy_finger)
    push!(results_x, x)
    push!(results_y, y)
    push!(results_z, (x * x) + (y * y) + (3 * x * y))
  end
  j1 = join(2, [2], 3, j0)
  j2 = join(2, [1], 3, j1)
  j3 = join(2, [1,2], 3, j2)
  j4(xx, yy, results_x, results_y, results_z) = begin 
    j3(xx, yy, finger(xx), finger(yy), results_x, results_y, results_z)
    results_z
  end
  (j0, j1, j2, j3, j4)
end

(j0, j1, j2, j3, j4) = make_polynomial()
xx = Relation((collect(0:100),collect(0:100)))
yy = Relation((collect(0:100), collect(reverse(0:100))))

@show @time j4(xx, yy, Int64[], Int64[], Int64[])

@code_warntype j0(xx, yy, RelationFinger{2}(1,1), RelationFinger{2}(1,1), Int64[], Int64[], Int64[])
@code_warntype j1(xx, yy, RelationFinger{2}(1,1), RelationFinger{1}(1,1), Int64[], Int64[], Int64[])
@code_warntype j2(xx, yy, RelationFinger{1}(1,1), RelationFinger{1}(1,1), Int64[], Int64[], Int64[])
@code_warntype j3(xx, yy, RelationFinger{0}(1,1), RelationFinger{0}(1,1), Int64[], Int64[], Int64[])

@code_warntype first(xx, RelationFinger{0}(1,1))

const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))
const polynomial = j4

@show @benchmark polynomial(big_xx, big_yy, Int64[], Int64[], Int64[])

end
