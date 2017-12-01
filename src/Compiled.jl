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

# index(fun)
# count(index, column)
# first(index, column)
# next(index, column)
# seek(index, column)
# get(index, column)

# implementation for tuple of column vectors

struct Relation{T <: Tuple}
  columns::T
end

function is_finite(::Type{Relation}) 
  true
end

struct RelationIndex{T <: Tuple}
  columns::T
  los::Vector{Int64} # inclusive
  his::Vector{Int64} # exclusive
end

function gallop{T}(column::AbstractArray{T}, lo::Int64, hi::Int64, value::T, threshold::Int64) ::Int64
  if (lo < hi) && cmp(column[lo], value) < threshold
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

@generated function index(fun::Relation{T}, ::Type{Val{order}}) where {T, order}
  @assert order == tuple(1:length(order)...) "Can't permute $order yet"
  n = length(T.parameters)
  quote
    RelationIndex(fun.columns, fill(1, $(n+1)), fill(length(fun.columns[1])+1, $(n+1)))
  end
end
 
function first(index::RelationIndex, ::Type{Val{C}}) where {C}
  index.his[C+1] = index.los[C]
end

function count(index::RelationIndex, ::Type{Val{C}}) where {C}
  # not actually correct, but will do for now
  index.his[C+1] - index.los[C+1]
end
 
function next(index::RelationIndex, ::Type{Val{C}}) where {C}
  column = index.columns[C]
  prev_hi = index.his[C]
  lo = index.his[C+1]
  if lo < prev_hi
    value = column[lo]
    hi = gallop(column, lo+1, prev_hi, value, 1)
    index.los[C+1] = lo
    index.his[C+1] = hi
    lo < hi
  else 
    false
  end
end

# TODO can push this into gallop, to search lo and hi at same time
#      or maybe use searchsorted if we can remove the cost of the subarray
function seek(index::RelationIndex, ::Type{Val{C}}, value) where {C}
  column = index.columns[C]
  prev_hi = index.his[C]
  lo = index.his[C+1]
  if lo < prev_hi
    lo = gallop(column, lo, prev_hi, value, 0)
    hi = gallop(column, lo+1, prev_hi, value, 1)
    index.los[C+1] = lo
    index.his[C+1] = hi
    lo < hi
  else 
    false
  end
end

function get(index::RelationIndex, ::Type{Val{C}}) where {C}
  index.columns[C][index.los[C+1]]
end
 
# TODO eventually we will just pass variables down the call stack, so we won't need this
function get_earlier(index::RelationIndex, ::Type{Val{C}}) where {C}
  index.columns[C][index.los[end]]
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
  indexes = [:(index(funs[$i], $(Val{tuple(sort_order...)}))) for (i, sort_order) in enumerate(sort_orders)]
  # TODO use inferred types to create Vector
  returns = [:([]) for _ in 1:(length(return_vars))]
  quote
    (funs) -> begin
      returns = tuple($(returns...))
      $tail(tuple($(indexes...)), returns)
      returns
    end
  end
end

# TODO handle repeated call_nums
function make_join(index_and_column_nums, tail)
  n = length(index_and_column_nums)
  index_nums = map((c) -> c[1], index_and_column_nums)
  column_nums = map((c) -> c[2], index_and_column_nums)
  quote 
    (indexes, returns) -> begin
      @nexprs $n (i) -> count_i = count(indexes[$index_nums[i]], Val{$column_nums[i]})
      min_count = @ncall $n min (i) -> count_i
      @nexprs $n (i) -> first(indexes[$index_nums[i]], Val{$column_nums[i]})
      @nif $n (min) -> count_min == min_count (min) -> begin
        while next(indexes[$index_nums[min]], Val{$column_nums[min]})
          let value = get(indexes[$index_nums[min]], Val{$column_nums[min]})
            if @nall $n (i) -> ((i == min) || seek(indexes[$index_nums[i]], Val{$column_nums[i]}, value))
              $tail(indexes, returns)
            end
          end
        end
      end
    end
  end
end

function make_return(num_funs, index_and_column_nums)
  pushes = [:(push!(returns[$i], get_earlier(indexes[$index_num], $(Val{column_num})))) for (i, (index_num, column_num)) in enumerate(index_and_column_nums)]
  quote
    (indexes, returns) -> begin
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
  call_and_column_nums = map(returned_vars) do var
    find_var(var, calls)
  end
  @show tail = eval(make_return(length(calls), call_and_column_nums))
  
  # make join functions
  # TODO assume everything is finite for now, figure out functions later
  # TODO assume no args passed for now ie all finitely supported
  for var in reverse(vcat(lambda.args, reduced_vars))
    call_and_column_nums = []
    for (call_num, call) in enumerate(calls)
      for (column_num, arg) in enumerate(call.args)
        if arg == var 
          push!(call_and_column_nums, (call_num, column_num))
        end
      end
    end
    @show call_and_column_nums
    @show tail = eval(make_join(call_and_column_nums, tail))
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
  j0(indexes, results) = begin
    x = get(indexes[1], Val{2})
    y = get(indexes[2], Val{2})
    push!(results[1], x)
    push!(results[2], y)
    push!(results[3], (x * x) + (y * y) + (3 * x * y))
  end
  j1 = eval(make_join([(2,2)], j0))
  j2 = eval(make_join([(1,2)], j1))
  j3 = eval(make_join([(1,1),(2,1)], j2))
  j4(xx, yy) = begin 
    results = (Int64[], Int64[], Int64[])
    indexes = (index(xx, Val{(1,2)}), index(yy, Val{(1,2)}))
    j3(indexes, results)
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

# @time p((little_xx, little_yy))
@time polynomial(little_xx, little_yy)

index(little_xx, Val{(1,2)})
 
using BenchmarkTools
# @show @benchmark polynomial(little_xx, little_yy)
# @show @benchmark polynomial(big_xx, big_yy)

end
