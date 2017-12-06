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

function is_finite(::Type{Relation{T}}) where {T}
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

function make_setup(sort_orders, return_var_types, tail)
  indexes = map(enumerate(sort_orders)) do i_sort_order
    i, sort_order = i_sort_order
    if sort_order != nothing
      sort_order_val = Val{tuple(sort_order...)}
      :(index(funs[$i], $sort_order_val))
    else
      :(funs[$i])
    end
  end
  returns = [:($typ[]) for typ in return_var_types]
  quote
    (funs) -> begin
      returns = tuple($(returns...))
      $tail(tuple($(indexes...)), returns)
      returns
    end
  end
end

macro all(args...)
  reduce((acc, arg) -> :($arg && $acc), true, reverse(map(esc, args)))
end

# TODO handle repeated call_nums
function make_seek(fun_and_var_nums, index_and_column_nums, num_vars, tail)
  n = length(index_and_column_nums)
  index_nums = map((c) -> c[1], index_and_column_nums)
  column_nums = map((c) -> c[2], index_and_column_nums)
  vars = [Symbol("var_$i") for i in 1:num_vars]
  calls = [:(indexes[$fun_num]($(vars[var_nums]...))) for (fun_num, var_nums) in fun_and_var_nums]
  tests = [:(value == $call) for call in calls]
  quote 
    (indexes, returns, $(vars...)) -> begin
      value = $(calls[1])
      if @all($(tests[2:end]...))
        if @nall $n (i) -> ((i == min) || seek(indexes[$index_nums[i]], Val{$column_nums[i]}, value))
          $tail(indexes, returns, $(vars...), value)
        end
      end
    end
  end
end

# TODO handle repeated call_nums
function make_join(index_and_column_nums, num_vars, tail)
  n = length(index_and_column_nums)
  index_nums = map((c) -> c[1], index_and_column_nums)
  column_nums = map((c) -> c[2], index_and_column_nums)
  vars = [Symbol("var_$i") for i in 1:num_vars]
  quote 
    (indexes, returns, $(vars...)) -> begin
      @nexprs $n (i) -> count_i = count(indexes[$index_nums[i]], Val{$column_nums[i]})
      min_count = @ncall $n min (i) -> count_i
      @nexprs $n (i) -> first(indexes[$index_nums[i]], Val{$column_nums[i]})
      @nif $n (min) -> count_min == min_count (min) -> begin
        while next(indexes[$index_nums[min]], Val{$column_nums[min]})
          let value = get(indexes[$index_nums[min]], Val{$column_nums[min]})
            if @nall $n (i) -> ((i == min) || seek(indexes[$index_nums[i]], Val{$column_nums[i]}, value))
              $tail(indexes, returns, $(vars...), value)
            end
          end
        end
      end
    end
  end
end

function make_return(num_funs, num_vars, return_var_nums)
    vars = [Symbol("var_$i") for i in 1:num_vars]
    pushes = [:(push!(returns[$i], $(Symbol("var_$return_var_num")))) for (i, return_var_num) in enumerate(return_var_nums)]
    quote
        (indexes, returns, $(vars...)) -> begin
          $(pushes...)
        end
    end
end

function compile(lambda::Lambda, fun_types::Dict{Symbol, Type}, var_types::Dict{Symbol, Type})
  # TODO handle reduce variable too
  returned_vars = vcat(lambda.args, lambda.value)
  
  # order variables by order of appearance in ast
  vars = union((call.args for call in lambda.domain)...)
  @show vars
  
  # permute all finite funs according to variable order
  sort_orders = Union{Vector{Int64}, Void}[]
  calls = Call[]
  for call in lambda.domain
      if is_finite(fun_types[call.fun])
        sort_order = Vector(1:length(call.args))
        sort!(sort_order, by=(ix) -> findfirst(vars, call.args[ix]))
        push!(sort_orders, sort_order)
        push!(calls, Call(call.fun, call.args[sort_order]))
      else
        push!(sort_orders, nothing)
        push!(calls, call)
      end
  end 
  
  # make return function
  # TODO just return everything for now, figure out reduce later
  returned_var_nums = map((var) -> findfirst(vars, var), returned_vars)
  @show tail = eval(@show make_return(length(calls), length(vars), returned_var_nums))
  
  # make join functions
  # TODO assume everything is finite for now, figure out functions later
  # TODO assume no args passed for now ie all finitely supported
  for (var_num, var) in reverse(collect(enumerate(vars)))
    index_and_column_nums = []
    fun_and_var_nums = []
    for (call_num, call) in enumerate(calls)
      for (column_num, arg) in enumerate(call.args)
        if arg == var 
          if is_finite(fun_types[call.fun])
            # use all finite funs
            push!(index_and_column_nums, (call_num, column_num))
          elseif column_num == length(call.args)
            # only use infinite funs if this is the return variable
            var_nums = map((arg) -> findfirst(vars, arg), call.args[1:end-1])
            push!(fun_and_var_nums, (call_num, var_nums))
          end
        end
      end
    end
    @show fun_and_var_nums
    if isempty(fun_and_var_nums)
      @show tail = eval(@show make_join(index_and_column_nums, var_num - 1, tail))
    else
      @show tail = eval(@show make_seek(fun_and_var_nums, index_and_column_nums, var_num - 1, tail))
    end
  end    
  
  returned_var_types = map((var) -> var_types[var], returned_vars)
  @show tail = eval(@show make_setup(sort_orders, returned_var_types, tail))

  tail
end

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast = Lambda(
  Ring{Int64}(+,*,1,0),
  [:x, :y],
  [
    Call(:xx, [:i, :x]), 
    Call(:yy, [:i, :y]), 
    Call(:zz, [:x, :y, :z]), 
  ],
  [:z]
  )

const xx = Relation((collect(0:1000),collect(0:1000)))
const yy = Relation((collect(0:1000), collect(reverse(0:1000))))
const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

fun_types = Dict{Symbol, Type}(:xx => typeof(xx), :yy => typeof(yy), :zz => typeof(zz))
var_types = Dict{Symbol, Type}(:i => Int64, :x => Int64, :y => Int64, :z => Int64)
const p = compile(polynomial_ast, fun_types, var_types)

@time p((xx, yy, zz))
 
using BenchmarkTools
# @show @benchmark p((big_xx, big_yy, zz))

end
