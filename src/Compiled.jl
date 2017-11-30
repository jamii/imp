module Compiled

using Base.Cartesian 
using Match

# interface for all functions

function is_function(fun_type)
  false
end 
# function call(fun, args...) end 

@generated function index(fun, ::Type{Val{order}}) where {order}
  @assert order == Vector(1:length(order))
  :fun
end

# interface for finite functions

function is_finite(fun_type) 
  false
end 
# function fingers(fun) end 
# function count(finger) end 
# function first(outer_finger, inner_finger, f) end
# function next(outer_finger, inner_finger, f) end
# function seek(outer_finger, inner_finger, value) end
# function get(outer_finger, inner_finger) end

# implementation for tuple of column vectors

struct Relation{T <: Tuple} 
  columns::T
end

mutable struct RelationFinger{T}
  column::Vector{T}
  lo::Int64 # inclusive
  hi::Int64 # exclusive
end

function gallop{T}(column::AbstractArray{T}, lo::Int64, hi::Int64, value::T, threshold::Int64) ::Int64
  @inbounds if (lo < hi) && cmp(column[lo], value) < threshold
    step = 1
    while (lo + step < hi) && cmp(column[lo + step], value) < threshold
      lo = lo + step
      step = step << 1
    end

    step = step >> 1
    while step > 0
      if (lo + step < hi) && cmp(column[lo + step], value) < threshold
        lo = lo + step
      end
      step = step >> 1
    end

    lo += 1
  end
  lo
end

function is_finite(::Type{Relation}) 
  true
end
  
@generated function fingers(fun::Relation{T}) where {T}
  n = length(T.parameters)
  fingers = [:(RelationFinger(fun.columns[$i], 1, l+1)) for i in 1:n]
  push!(fingers, :(RelationFinger(Vector{Tuple{}}(l), 1, l+1)))
  quote
    l = length(fun.columns[1])
    tuple($(fingers...))
  end
end

function count(finger::RelationFinger)
  # not actually correct, but will do for now
  finger.hi - finger.lo
end

function first(outer_finger::RelationFinger, inner_finger::RelationFinger) ::Bool
  inner_finger.hi = outer_finger.lo
  next(outer_finger, inner_finger)
end

function next(outer_finger::RelationFinger, inner_finger::RelationFinger) ::Bool
  column = outer_finger.column
  lo = inner_finger.hi
  if lo < outer_finger.hi
    hi = gallop(column, lo, outer_finger.hi, column[lo], 1)
    inner_finger.lo = lo
    inner_finger.hi = hi
    lo < hi
  else
    false
  end
end

# TODO can push this into gallop, to search lo and hi at same time
#      or maybe use searchsorted if we can remove the cost of the subarray
function seek(outer_finger::RelationFinger{T}, inner_finger::RelationFinger, value::T) ::Bool where {T}
  column = outer_finger.column
  lo = inner_finger.hi
  if lo < outer_finger.hi
    lo = gallop(column, inner_finger.hi, outer_finger.hi, value, 0)
    hi = gallop(column, lo+1, outer_finger.hi, value, 1)
    inner_finger.lo = lo
    inner_finger.hi = hi
    lo < hi
  else
    false
  end
end

function get(outer_finger::RelationFinger{T}, inner_finger::RelationFinger) ::T where {T}
  outer_finger.column[inner_finger.lo]
end

function _join_fingers(num_fingers)
  outer_fingers = [Symbol("outer_fingers_$i") for i in 1:num_fingers]
  inner_fingers = [Symbol("inner_fingers_$i") for i in 1:num_fingers]
  quote 
    function join_fingers($(outer_fingers...), $(inner_fingers...), f::Function)
      @nexprs $num_fingers (i) -> count_i = count(outer_fingers_i)
      min_count = @ncall $num_fingers min (i) -> count_i
      @nif $num_fingers (i) -> count_i == min_count (i) -> begin
        if @nall $num_fingers (j) -> first(outer_fingers_j, inner_fingers_j)
          while true
            value = get(outer_fingers_i, inner_fingers_i)
            if (@nall $num_fingers (j) -> ((i == j) || seek(outer_fingers_j, inner_fingers_j, value)))
              f()
            end
            if !next(outer_fingers_i, inner_fingers_i)
              break
            end
          end
        end
      end
    end
  end
end

for i in 1:3
  eval(_join_fingers(i))
end

struct Ring{T}
  add::Function
  mult::Function
  one::T
  zero::T
end

struct Call
  fun # function, or anything which implements finite function interface
  args::Vector{Symbol}
end

struct Lambda
  ring::Ring
  args::Vector{Symbol}
  domain::Vector{Call}
  value::Vector{Symbol}
end
  
function find_var(var::Symbol, calls::Vector{Call})
  for (call_num, call) in enumerate(calls)
    for (arg_num, arg) in enumerate(call.args)
      if arg == var
        return (call_num, arg_num)
      end
    end
  end
  error("Not found")
end

function compile(lambda::Lambda)
  reduced_vars = setdiff(union((call.args for call in lambda.domain)...), lambda.args)
  
  sort_orders = Vector{Int64}[]
  calls = Call[]
  for call in lambda.domain
    sort_order = Vector(1:length(call.args))
    sort!(sort_order, by=(ix) -> findfirst(reduced_vars, call.args[ix]))
    push!(sort_orders, sort_order)
    push!(calls, Call(call.fun, call.args[sort_order]))
  end 
  
  code = []
  
  for result_num in 1:(1+length(lambda.args))
    result_name = Symbol("results_$(result_num)") 
    push!(code, :(const $result_name = [])) # TODO type
  end
  
  for (call_num, (sort_order, call)) in enumerate(zip(sort_orders, calls))
    sort_order_val = Val{tuple(sort_order...)}
    fingers = [Symbol("finger_$(call_num)_$(finger_num)") for finger_num in 1:(1+length(call.args))]
    push!(code, :(const ($(fingers...)) = fingers(index($(call.fun), $sort_order_val))))
  end
  
  for (var_num, var) in enumerate(reduced_vars)
    call_and_finger_nums = []
    for (call_num, call) in enumerate(calls)
      finger_num = findfirst(call.args, var)
      if finger_num != 0
        push!(call_and_finger_nums, (call_num, finger_num))
      end
    end
    outer_fingers = [Symbol("finger_$(call_num)_$(finger_num)") for (call_num, finger_num) in call_and_finger_nums]
    inner_fingers = [Symbol("finger_$(call_num)_$(finger_num+1)") for (call_num, finger_num) in call_and_finger_nums]
    this_step = Symbol("step_$(var_num)")
    next_step = Symbol("step_$(var_num+1)")
    push!(code, :(const $this_step = () -> join_fingers($(outer_fingers...), $(inner_fingers...), $next_step)))
  end
  
  num_steps = length(reduced_vars)+1
  last_step = Symbol("step_$(num_steps)")
  get_vars = []
  for var in union(lambda.value, lambda.args)
    (call_num, finger_num) = find_var(var, calls)
    outer_finger = Symbol("finger_$(call_num)_$(finger_num)")
    inner_finger = Symbol("finger_$(call_num)_$(finger_num+1)")
    push!(get_vars, :(const $var = get($outer_finger, $inner_finger)))
  end
  push_vars = []
  for (var_num, var) in enumerate(lambda.args)
    result = Symbol("results_$(var_num)")
    push!(push_vars, :(push!($result, $var)))
  end
  num_results = length(lambda.args)+1
  last_result = Symbol("results_$(num_results)")
  push_value = :(push!($last_result, $(lambda.ring.mult)($(lambda.value...))))
  push!(code, :(const $last_step = () -> begin
    $(get_vars...)
    $(push_vars...)
    $push_value
  end))
  
  quote
    $(code...)
  end
end

# polynomial_ast = Lambda(
#   Ring{Int64}(+,*,1,0),
#   [:i],
#   [
#     Call(:xx, [:i, :x]), 
#     Call(:yy, [:i, :y]), 
#     Call((x,y) -> (x * x) + (y * y) + (3 * x * y), [:x, :y, :z]), 
#   ],
#   [:z]
#   )
# 
# compile(polynomial_ast)

using BenchmarkTools

function polynomial(xx, yy)
  const results_x = Int64[]
  const results_y = Int64[]
  const results_z = Int64[]
  
  const (x1, x2, x3) = fingers(xx)
  const (y1, y2, y3) = fingers(yy)
  
  # @code_warntype join_fingers_2(j1o, j1i, () -> ())
  
  const n3 = () -> begin
    x = get(x2, x3)
    y = get(y2, y3)
    z = (x * x) + (y * y) + (3 * x * y)
    push!(results_x, x)
    push!(results_y, y)
    push!(results_z, z)
  end
  const n2 = () -> begin
    join_fingers(y2, y3, n3)
  end
  const n1 = () -> begin
    join_fingers(x2, x3, n2)
  end
  join_fingers(x1, y1, x2, y2, n1)
  
  results_z
end

# xx = Relation((collect(1:100),collect(1:100)))
# yy = Relation((collect(1:100), collect(reverse(1:100))))
# 
# @show @time polynomial(xx, yy)
# @code_warntype polynomial(xx, yy)
# 
# 
# const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
# const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

# @show @benchmark polynomial(big_xx, big_yy)

end
