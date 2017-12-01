module Compiled

using Base.Cartesian 
using Match

# interface for all functions

function is_function(fun_type)
  false
end 
# fun(args...) end 

# interface for finite functions

function is_finite(fun_type) 
  false
end 

@generated function index(fun, ::Type{Val{order}}) where {order}
  @assert order == tuple(1:length(order)...) "Can't permute $order yet"
  :fun
end

# function first(fun) end 
# function first(fun, finger) end
# function count(fun, finger) end 
# function next(fun, outer_finger, inner_finger) end
# function seek(fun, outer_finger, inner_finger, value) end
# function get(fun, finger) end

# implementation for tuple of column vectors

struct Relation{T <: Tuple} 
  columns::T
end

struct RelationFinger{Column}
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

function first(fun::Relation)
  RelationFinger{0}(1, length(fun.columns[1]) + 1)
end
 
@generated function first(fun::Relation, finger::RelationFinger{C}) where {C}
  C2 = C + 1
  quote
    RelationFinger{$C2}(0, finger.lo)
  end
end

function count(fun::Relation, finger::RelationFinger)
  # not actually correct, but will do for now
  finger.hi - finger.lo
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
@generated function seek(fun::Relation, outer_finger::RelationFinger{C}, inner_finger::RelationFinger{C2}, value) where {C, C2}
  @assert C2 == C + 1
  quote
    column = fun.columns[C2]
    lo = inner_finger.hi
    if lo < outer_finger.hi
      lo = gallop(column, lo, outer_finger.hi, value, 0)
      hi = gallop(column, lo+1, outer_finger.hi, value, 1)
      RelationFinger{C2}(lo, hi)
    else
      RelationFinger{C2}(lo, outer_finger.hi)
    end
  end
end
 
function get(fun::Relation, finger::RelationFinger{C}) where {C}
  fun.columns[C][finger.lo]
end

# TODO eventually we will just pass variables down the call stack, so we won't need this
function get(fun::Relation, finger::RelationFinger{C}, ::Type{Val{C2}}) where {C, C2}
  fun.columns[C2][finger.lo]
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

function make_setup(sort_orders, return_vars, tail)
  funs = [Symbol("fun_$i") for i in 1:length(sort_orders)]
  indexes = [:(index($fun, $(Val{tuple(sort_order...)}))) for (fun, sort_order) in zip(funs, sort_orders)]
  fingers = [:(first($fun)) for fun in funs]
  # TODO use inferred types to create Vector
  returns = [:([]) for _ in 1:(length(return_vars))]
  quote
    ($(funs...),) -> begin
      returns = tuple($(returns...))
      $tail($(indexes...), $(fingers...), returns)
      returns
    end
  end
end

# TODO handle repeated call_nums
function make_join(num_funs, call_nums, tail)
  n = length(call_nums)
  funs = [Symbol("ignored_fun_$i") for i in 1:num_funs]
  outer_fingers = [Symbol("ignored_outer_finger_$i") for i in 1:num_funs]
  result_fingers = [Symbol("ignored_outer_finger_$i") for i in 1:num_funs]
  for i in 1:length(call_nums)
    funs[call_nums[i]] = Symbol("fun_$i")
    outer_fingers[call_nums[i]] = Symbol("outer_finger_$i")
    result_fingers[call_nums[i]] = Symbol("inner_finger_$i")
  end
  inner_fingers = [Symbol("inner_finger_$i") for i in 1:length(call_nums)]
  quote 
    ($(funs...), $(outer_fingers...), returns) -> begin
      @nexprs $n (i) -> count_i = count(fun_i, outer_finger_i)
      min_count = @ncall $n min (i) -> count_i
      @nexprs $n (i) -> inner_finger_i = first(fun_i, outer_finger_i)
      @nif $n (min) -> count_min == min_count (min) -> begin
        while true
          inner_finger_min = next(fun_min, outer_finger_min, inner_finger_min)
          if count(fun_min, inner_finger_min) == 0
            break
          end
          let value = get(fun_min, inner_finger_min)
            if (@nall $n (i) -> ((i == min) || begin
              inner_finger_i = seek(fun_i, outer_finger_i, inner_finger_i, value)
              count(fun_i, inner_finger_i) > 0
            end))
              $tail($(funs...), $(result_fingers...), returns)
            end
          end
        end
      end
    end
  end
end

function make_return(num_funs, call_and_finger_nums)
  funs = [Symbol("fun_$i") for i in 1:num_funs]
  fingers = [Symbol("finger_$i") for i in 1:num_funs]
  pushes = [:(push!(returns[$call_num], get($(funs[call_num]), $(fingers[call_num]), $(Val{finger_num})))) for (call_num, finger_num) in call_and_finger_nums]
  quote
    ($(funs...), $(fingers...), returns) -> begin
      $(pushes...)
    end
  end
end

function compile(lambda::Lambda, inferred_type::Function)
  # TODO handle reduce variable too
  returned_vars = lambda.args
  
  # pick a variable order
  reduced_vars = setdiff(union((call.args for call in lambda.domain)...), lambda.args)
  
  # permute all finite funs accordingly
  sort_orders = Vector{Int64}[]
  calls = Call[]
  for call in lambda.domain
    sort_order = Vector(1:length(call.args))
    sort!(sort_order, by=(ix) -> findfirst(reduced_vars, call.args[ix]))
    push!(sort_orders, sort_order)
    push!(calls, Call(call.fun, call.args[sort_order]))
  end 
  
  # TODO rename funs and vars. only setup needs to know correct names
  
  # make return function
  # TODO just return everything for now, figure out reduce later
  call_and_finger_nums = map(returned_vars) do var
    find_var(var, calls)
  end
  @show tail = eval(make_return(length(calls), call_and_finger_nums))
  
  # make join functions
  # TODO assume everything is finite for now, figure out functions later
  # TODO assume no args passed for now ie all finitely supported
  for var in vcat(lambda.args, reduced_vars)
    call_nums = find(calls) do call
      any(call.args) do arg
        arg == var
      end
    end
    @show var call_nums
    @show tail = eval(make_join(length(calls), call_nums, tail))
  end    
  
  @show tail = eval(make_setup(sort_orders, lambda.args, tail))

  tail
end

polynomial_ast = Lambda(
  Ring{Int64}(+,*,1,0),
  [:i, :x, :y],
  [
    Call(:xx, [:i, :x]), 
    Call(:yy, [:i, :y]), 
    # Call((x,y) -> (x * x) + (y * y) + (3 * x * y), [:x, :y, :z]), 
  ],
  [:z]
  )

p = compile(polynomial_ast, (sym) -> typeof(eval(sym)))

function make_polynomial()
  j0(xx, yy, xx_finger, yy_finger, results) = begin
    x = get(xx, xx_finger)
    y = get(yy, yy_finger)
    push!(results[1], x)
    push!(results[2], y)
    push!(results[3], (x * x) + (y * y) + (3 * x * y))
  end
  j1 = eval(make_join(2, [2], j0))
  j2 = eval(make_join(2, [1], j1))
  j3 = eval(make_join(2, [1,2], j2))
  j4(xx, yy, results) = begin 
    j3(xx, yy, first(xx), first(yy), results)
    results[3]
  end
  (j0, j1, j2, j3, j4)
end

(j0, j1, j2, j3, j4) = make_polynomial()
xx = Relation((collect(0:100), collect(0:100)))
yy = Relation((collect(0:100), collect(reverse(0:100))))
const polynomial = j4

const little_xx = Relation((collect(0:1000),collect(0:1000)))
const little_yy = Relation((collect(0:1000), collect(reverse(0:1000))))
const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

# @code_warntype j0(xx, yy, RelationFinger{2}(1,1), RelationFinger{2}(1,1), (Int64[], Int64[], Int64[]))
# @code_warntype j1(xx, yy, RelationFinger{2}(1,1), RelationFinger{1}(1,1), (Int64[], Int64[], Int64[]))
# @code_warntype j2(xx, yy, RelationFinger{1}(1,1), RelationFinger{1}(1,1), (Int64[], Int64[], Int64[]))
# @code_warntype j3(xx, yy, RelationFinger{0}(1,1), RelationFinger{0}(1,1), (Int64[], Int64[], Int64[]))

@show @time p(little_xx, little_yy)
 
using BenchmarkTools
# @show @benchmark polynomial(little_xx, little_yy, (Int64[], Int64[], Int64[]))
# @show @benchmark polynomial(big_xx, big_yy, (Int64[], Int64[], Int64[]))

end
