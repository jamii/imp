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
  args::Vector{Any}
end

struct Lambda
  ring::Ring
  args::Vector{Symbol}
  domain::Vector{Call}
  value::Vector{Any} 
end

struct Index
  num::Int64 # position in the index tuple TODO remove this hack
  fun 
  sort_order::Vector{Int64}
end

struct SumProduct
  ring::Ring
  var::Symbol
  domain::Vector{Call}
  value::Vector{Union{SumProduct, Symbol}} 
end

function make_setup(indexes, tail)
  index_inits = [:(index(funs[$(Expr(:quote, index.fun))], $(Val{tuple(index.sort_order...)}))) for index in indexes]
  quote
    (funs) -> $tail(tuple($(index_inits...)))
  end
end

macro all(args...)
  reduce((acc, arg) -> :($arg && $acc), true, reverse(map(esc, args)))
end

function make_seek(fun_and_var_nums, index_and_column_nums, num_vars, tail)
  sort!(index_and_column_nums) # ensures that repeated variables are seeked in the correct order
  n = length(index_and_column_nums)
  index_nums = map((c) -> c[1], index_and_column_nums)
  column_nums = map((c) -> c[2], index_and_column_nums)
  is_repeated = [(i > 1) && (index_nums[i] == index_nums[i-1]) for i in 1:n]
  vars = [Symbol("var_$i") for i in 1:num_vars]
  calls = [:($fun($(vars[var_nums]...))) for (fun, var_nums) in fun_and_var_nums]
  tests = [:(value == $call) for call in calls]
  quote
    (indexes, returns, $(vars...)) -> begin
      value = $(calls[1])
      if value != nothing # handles partial functions
        if @all($(tests[2:end]...))
          if @nall $n (i) -> begin
            first(indexes[$index_nums[i]], Val{$column_nums[i]})
            seek(indexes[$index_nums[i]], Val{$column_nums[i]}, value)
          end
            $tail(indexes, returns, $(vars...), value)
          end
        end
      end
    end
  end
end

@inline function first_if(bool, index, column)
  if bool
    first(index, column)
  end
end

@inline function seek_if(bool, index, column, value)
  first_if(bool, index, column)
  seek(index, column, value)
end

function make_join(index_and_column_nums, num_vars, tail)
  sort!(index_and_column_nums) # ensures that repeated variables are seeked in the correct order
  n = length(index_and_column_nums)
  index_nums = map((c) -> c[1], index_and_column_nums)
  column_nums = map((c) -> c[2], index_and_column_nums)
  is_repeated = [(i > 1) && (index_nums[i] == index_nums[i-1]) for i in 1:n]
  vars = [Symbol("var_$i") for i in 1:num_vars]
  quote
    (indexes, returns, $(vars...)) -> begin
      @nexprs $n (i) -> count_i = if $is_repeated[i]
        typemax(Int64) # we can't iter over a repeated variable, so make sure we don't pick it
      else
        count(indexes[$index_nums[i]], Val{$column_nums[i]})
      end
      min_count = @ncall $n min (i) -> count_i
      @nexprs $n (i) -> first_if(!$is_repeated[i], indexes[$index_nums[i]], Val{$column_nums[i]})
      @nif $n (min) -> count_min == min_count (min) -> begin
        while next(indexes[$index_nums[min]], Val{$column_nums[min]})
          let value = get(indexes[$index_nums[min]], Val{$column_nums[min]})
            if @nall $n (i) -> ((i == min) || seek_if($is_repeated[i], indexes[$index_nums[i]], Val{$column_nums[i]}, value))
              $tail(indexes, returns, $(vars...), value)
            end
          end
        end
      end
    end
  end
end

function compile(ir::SumProduct, vars::Vector{Symbol}) ::Function
  # make code for calculating value returned by each iteration
  tail_vars = push!(copy(vars), ir.var)
  tails = []
  for value in ir.value
    @match value begin
      SumProduct => push!(tails, :($(compile(value, tail_vars))($(tail_vars...))))
      Symbol => push!(tails, value)
    end
  end
  tail = :(@product($(tails...)))
  
  # check for repeated repeated variables eg foo(x,x) which need to be handled specially
  index_and_column_nums = (Int64, Int64)[]
  for call in ir.domain
    if call.fun isa Index
      push!(index_and_column_nums, (call.fun.num, length(call.args)))
    end
  end
  is_repeat = map(ir.domain) do call
    (call.fun isa Index) &&
      contains(index_and_column_nums, (call.fun.num, length(call.args)-1))
  end
    
  # make code for setting up and testing each call
  setups = []
  tests = []
  for (call_num, call) in enumerarate(ir.domain) 
    if call.fun isa Index
      if is_repeat[call_num]
        push!(setups, nothing)
        push!(tests, quote
          first(indexes[$(call.fun.num)], $(Val{length(call.args)}))
          seek(indexes[$(call.fun.num)], $(Val{length(call.args)}), $(ir.var))
        end)
      else
        push!(setups, :(first(indexes[$(call.fun.num)], $(Val{length(call.args)}))))
        push!(tests, :(seek(indexes[$(call.fun.num)], $(Val{length(call.args)}), $(ir.var))))
      end
    else
      push!(setups, nothing)
      push!(tests, :($(call.fun)($(call.args[1:end-1]...)) = $(call.args[end])))
    end
  end
  
  # CONTINUE HERE
end

function Compiled.factorize(lambda::Lambda, vars::Vector{Symbol}) ::SumProduct
  # insert indexes and partial calls
  calls = Call[]
  indexes = Index[]
  for call in lambda.domain
    if is_finite(fun_type(call.fun))
      # sort args according to variable order
      n = length(call.args)
      sort_order = Vector(1:n)
      sort!(sort_order, by=(ix) -> findfirst(vars, call.args[ix]))
      index = Index(length(indexes) + 1, call.fun, sort_order)
      push!(indexes, index)
      # insert all prefixes of args
      for i in 1:n
        push!(calls, Call(index, call.args[sort_order][1:i]))
      end
    else
      push!(calls, call)
    end
  end

  # make join functions
  latest_var_nums = map(calls) do call
    maximum(call.args) do arg 
      findfirst(vars, arg)
    end
  end
  
  sum_products = map(reverse(1:length(vars))) do var_num
    var = vars[var_num]
    domain = calls[latest_var_nums .== var_num]
    value = (var in lambda.value) ? [var] : []
    SumProduct(lambda.ring, var, domain, value)
  end
  
  # stitch them all together
  reduce(sum_products) do tail, sum_product
    push!(sum_product.value, tail)
    sum_product
  end
end

function handle_constants(lambda::Lambda) ::Lambda
  constants = Call[]
  
  handle_constant = (arg) -> begin
    if isa(arg, Symbol)
      arg
    else
      var = gensym("constant")
      push!(constants, Call(() -> arg, [var]))
      var
    end
  end
  
  domain = map(lambda.domain) do call
    Call(call.fun, map(handle_constant, call.args))
  end
  
  value = map(handle_constant, lambda.value)
  
  Lambda(lambda.ring, lambda.args, vcat(constants, domain), value)
end

function compile(lambda::Lambda, fun_type::Function, var_type::Function)
  lambda = handle_constants(lambda)
  generate(lambda, fun_type, var_type)
end

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast1 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:x, :y],
  [
    Call(:xx, [:i, :x]),
    Call(:yy, [:i, :y]),
    Call(:zz, [:x, :y, :z]),
  ],
  [:z]
  )

polynomial_ast2 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:x, :y],
  [
    Call(:xx, [:x, :x]),
    Call(:yy, [:x, :y]),
    Call(:zz, [:x, :y, :z]),
  ],
  [:z]
  )
  
zz(x, y) = (x * x) + (y * y) + (3 * x * y)
  
polynomial_ast3 = Lambda(
    Ring{Int64}(+,*,1,0),
    [:x, :y],
    [  
      Call(:xx, [:i, :x]),
      Call(:yy, [:i, :y]),
      Call(*, [:x, :x, :t1]),
      Call(*, [:y, :y, :t2]),
      Call(*, [3, :x, :y, :t3]),
      Call(+, [:t1, :t2, :t3, :z])
    ],
    [:z]
    )

const xx = Relation((collect(0:1000),collect(0:1000)))
const yy = Relation((collect(0:1000), collect(reverse(0:1000))))
const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

fun_type(fun) = typeof(eval(fun))
var_type = Dict(:i => Int64, :x => Int64, :y => Int64, :z => Int64)
# const p1 = compile(polynomial_ast1, fun_type, (var) -> var_type[var])
# const p2 = compile(polynomial_ast2, fun_type, (var) -> var_type[var])
# const p3 = compile(polynomial_ast3, fun_type, (var) -> var_type[var])

# inputs = Dict(:xx => xx, :yy => yy, :zz => zz)
# @time p1(inputs)
# @time p2(inputs)
# @time p3(inputs)
# @assert p1(inputs) == p2(inputs)
# @assert p1(inputs) == p3(inputs)

# using BenchmarkTools
# big_inputs = Dict(:xx => big_xx, :yy => big_yy, :zz => zz)
# @show @benchmark p1(big_inputs)
# @show @benchmark p2(big_inputs)
# @show @benchmark p3(big_inputs)

end
