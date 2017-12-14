module Compiled

using Base.Cartesian
using Match

function simplify_expr(expr::Expr)
  @match expr begin
    Expr(:block, lines, _) => begin
      flattened = []
      for line in map(simplify_expr, lines)
        @match line begin
          Expr(:block, more_lines, _) => append!(flattened, more_lines)
          Expr(:line, _, _) => nothing
          _ => push!(flattened, line)
        end
      end
      if length(flattened) == 1
        flattened[1]
      else
        Expr(:block, flattened...)
      end
    end
    Expr(head, args, _) => Expr(simplify_expr(head), map(simplify_expr, args)...)
  end
end

function simplify_expr(other)
  other
end

# interface for all functions

function is_function(fun_type)
  false
end
# fun(args...) end

# interface for finite functions

function can_index(fun_type)
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

function can_index(::Type{Relation{T}}) where {T}
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

struct Ring{T}
  add::Function
  mult::Function
  one::T
  zero::T
end

Base.eltype(ring::Ring{T}) where {T} = T

const count_ring = Ring(+, *, 1, 0)

struct Var
  name::Symbol
end

struct Call{T} # type of fun
  fun # function, or anything which implements finite function interface
  args::Vector{Any}
end

struct Lambda
  ring::Ring
  args::Vector{Symbol}
  domain::Vector{Call}
  value::Vector{Any} 
end

struct Index{T} # type of fun
  name::Symbol
  fun 
  permutation::Vector{Int64}
end

struct SumProduct
  ring::Ring
  var::Symbol
  domain::Vector{Call}
  value::Vector{Union{SumProduct, Symbol}} 
end

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

function inline(function_expr::Expr, value)
  @match function_expr begin
    Expr(:->, [var::Symbol, body], _)  => quote
      let $var = $value
        $body
      end
    end
    _ => error("Can't inline $function_expr")
  end
end

function get_index(fun, index::Index{Relation{T}}) where {T}
  @assert index.permutation == collect(1:length(index.permutation)) "Can't permute $(index.permutation) yet"
  n = length(T.parameters)
  quote
    fun = $(esc(fun))
    RelationIndex(fun.columns, fill(1, $(n+1)), fill(length(fun.columns[1])+1, $(n+1)))
  end
end

macro get_index(fun, index)
  get_index(fun, index)
end

macro count(call::Call)
  if isa(call.fun, Index)
    quote 
      count($(esc(call.fun.name)), $(Val{length(call.args)}))
    end
  else
    1
  end
end

macro value(call::Call)
  quote
    $(esc(call.fun))($(map(esc, call.args)...))
  end
end

macro value(var::Var)
  esc(var.name)
end

macro prepare(call::Call)
  if isa(call.fun, Index) 
    quote 
      first($(esc(call.fun.name)), $(Val{length(call.args)}))
    end
  else
    nothing
  end
end

macro test(call::Call)
  if isa(call.fun, Index) 
    quote 
      seek($(esc(call.fun.name)), $(Val{length(call.args)}), $(esc(call.args[end])))
    end
  else
    quote
      $(esc(call.fun))($(map(esc, call.args[1:end-1])...)) == $(esc(call.args[end]))
    end
  end
end

macro product(ring::Ring, domain::Vector{Call}, value::Vector{Union{Call, Var}})
  code = :result
  for call in reverse(value)
    code = quote
      result = $(ring.mult)(result, @value($call))
      if result == $(ring.zero)
        $(ring.zero)
      else 
        $code
      end
    end
  end
  for call in reverse(domain)
    code = quote
      if !@test($call)
        $(ring.zero)
      else
        $code
      end
    end
  end
  quote
    result = $(ring.one)
    $code
  end
end

macro iter(call::Call, f)
  # TODO dispatch on type of fun, rather than index/not-index
  value = gensym("value")
  if isa(call.fun, Index)
    quote 
      while next($(esc(call.fun.name)), $(Val{length(call.args)}))
        $(esc(value)) = get($(esc(call.fun.name)), $(Val{length(call.args)}))
        $(esc(inline(f, value)))
      end
    end
  else
    quote
      $(esc(value)) = $(esc(call.fun))($(map(esc, call.args[1:end-1])...))
      $(esc(inline(f, value)))
    end
  end
end

macro sum(ring::Ring, call::Call, f)
  value = gensym("value")
  quote
    result = $(ring.zero)
    @iter($call, ($(esc(value))) -> begin
      result = $(ring.add)(result, $(esc(inline(f, value))))
    end)
    result
  end
end

macro join(ir::SumProduct, value::Vector{Union{Call, Var}})
  @assert !isempty(ir.domain) "Cant join on an empty domain"
  quote
    $(@splice call in ir.domain quote
      @prepare($call)
    end)
    mins = tuple($(@splice call in ir.domain quote
      @count($call)
    end))
    min = Base.min(mins...)
    $(@splice i in 1:length(ir.domain) quote
      if mins[$i] == min
        return @sum($(ir.ring), $(ir.domain[i]), ($(esc(ir.var))) -> begin
          @product($(ir.ring), $(ir.domain[1:length(ir.domain) .!= i]), $value)
        end)
      end
      error("Impossibles!")
    end)
  end
end

macro define_joins(name, vars::Vector{Symbol}, ir::SumProduct)
  child_vars = union(vars, [ir.var])
  value = map(ir.value) do v
    if isa(v, SumProduct)
      child_name = gensym("join")
      Call{Function}(child_name, child_vars)
    else
      Var(v)
    end
  end
  quote
    $(@splice (old_v, new_v) in zip(ir.value, value) begin
      if isa(old_v, SumProduct)
        :(@define_joins($(new_v.fun), $(convert(Vector{Symbol}, new_v.args)), $old_v))
      end
    end)
    function $(esc(name))($(map(esc, vars)...))
      @join($ir, $(convert(Vector{Union{Call, Var}}, value)))
    end
  end
end

macro callable(lambda::Lambda, ir::SumProduct, indexes::Vector{Index}) 
  name = gensym("lambda")
  child_name = gensym("join")
  index_names = map((index) -> index.name, indexes)
  quote
    @define_joins($child_name, $(index_names), $ir)
    function $name(env)
      $child_name($(@splice index in indexes quote
        @get_index(env[$(Expr(:quote, index.fun))], $index)
      end))
    end
  end
end

function Compiled.factorize(lambda::Lambda, vars::Vector{Symbol}) ::Tuple{SumProduct, Vector{Index}}
  # insert indexes and partial calls
  calls = Call[]
  indexes = Index[]
  for call in lambda.domain
    typ = fun_type(call.fun)
    if can_index(typ)
      # sort args according to variable order
      n = length(call.args)
      permutation = Vector(1:n)
      sort!(permutation, by=(ix) -> findfirst(vars, call.args[ix]))
      index = Index{typ}(gensym("index"), call.fun, permutation)
      push!(indexes, index)
      # insert all prefixes of args
      for i in 1:n
        push!(calls, Call{typ}(index, call.args[permutation][1:i]))
      end
    else
      push!(calls, call)
    end
  end

  # make individual SumProducts
  latest_var_nums = map(calls) do call
    maximum(call.args) do arg 
      findfirst(vars, arg)
    end
  end
  sum_products = map(1:length(vars)) do var_num
    var = vars[var_num]
    domain = calls[latest_var_nums .== var_num]
    value = (var in lambda.value) ? [var] : []
    SumProduct(lambda.ring, var, domain, value)
  end
  
  # stitch them all together
  ir = reduce(reverse(sum_products)) do tail, sum_product
    push!(sum_product.value, tail)
    sum_product
  end
  
  (ir, indexes)
end

function order_vars(lambda::Lambda) ::Vector{Symbol}
  # just use order vars appear in the ast for now
  union(map((call) -> call.args, lambda.domain)...)
end

function lower_constants(lambda::Lambda) ::Lambda
  constants = Call[]
  
  lower_constant = (arg) -> begin
    if isa(arg, Symbol)
      arg
    else
      var = gensym("constant")
      fun = () -> arg
      push!(constants, Call{typeof(fun)}(fun, [var]))
      var
    end
  end
  
  domain = map(lambda.domain) do call
    typeof(call)(call.fun, map(lower_constant, call.args))
  end
  
  value = map(lower_constant, lambda.value)
  
  Lambda(lambda.ring, lambda.args, vcat(constants, domain), value)
end

function compile(lambda::Lambda, fun_type::Function, var_type::Function)
  lambda = lower_constants(lambda)
  vars = order_vars(lambda)
  ir, indexes = factorize(lambda, vars)
  code = quote
    @callable($lambda, $ir, $indexes)
  end
  # @show simplify_expr(macroexpand(code))
  eval(code)
end

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast1 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:i, :x, :y, :z],
  [
    Call{Any}(:xx, [:i, :x]),
    Call{Any}(:yy, [:i, :y]),
    Call{Any}(:zz, [:x, :y, :z]),
  ],
  [:z]
  )

polynomial_ast2 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:x, :y, :z],
  [
    Call{Any}(:xx, [:x, :x]),
    Call{Any}(:yy, [:x, :y]),
    Call{Any}(:zz, [:x, :y, :z]),
  ],
  [:z]
  )
  
zz(x, y) = (x * x) + (y * y) + (3 * x * y)
  
polynomial_ast3 = Lambda(
    Ring{Int64}(+,*,1,0),
    [:i, :x, :y, :t1, :t2, :t3, :z],
    [  
      Call{Any}(:xx, [:i, :x]),
      Call{Any}(:yy, [:i, :y]),
      Call{Any}(*, [:x, :x, :t1]),
      Call{Any}(*, [:y, :y, :t2]),
      Call{Any}(*, [3, :x, :y, :t3]),
      Call{Any}(+, [:t1, :t2, :t3, :z])
    ],
    [:z]
    )
const xx = Relation((collect(0:1000),collect(0:1000)))
const yy = Relation((collect(0:1000), collect(reverse(0:1000))))
const big_xx = Relation((collect(0:1000000),collect(0:1000000)))
const big_yy = Relation((collect(0:1000000), collect(reverse(0:1000000))))

fun_type(fun) = typeof(eval(fun))
var_type = Dict(:i => Int64, :x => Int64, :y => Int64, :z => Int64)
const p1 = compile(polynomial_ast1, fun_type, (var) -> var_type[var])
const p2 = compile(polynomial_ast2, fun_type, (var) -> var_type[var])
const p3 = compile(polynomial_ast3, fun_type, (var) -> var_type[var])

inputs = Dict(:xx => xx, :yy => yy, :zz => zz)
@show sum(((x * x) + (y * y) + (3 * x * y) for (x,y) in zip(xx.columns[2], yy.columns[2])))
@show @time p1(inputs)
@show @time p2(inputs)
@show @time p3(inputs)
@assert p1(inputs) == p2(inputs)
@assert p1(inputs) == p3(inputs)

# using BenchmarkTools
# big_inputs = Dict(:xx => big_xx, :yy => big_yy, :zz => zz)
# @show @benchmark p1(big_inputs)
# @show @benchmark p2(big_inputs)
# @show @benchmark p3(big_inputs)

end
