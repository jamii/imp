module Compiled

using Base.Cartesian
using Match

struct Ring{T}
  add::Function
  mult::Function
  one::T
  zero::T
end

const count_ring = Ring(+, *, 1, 0)

struct Constant
  value::Any
end

struct FunCall
  name::Union{Symbol, Function}
  typ::Type
  args::Vector{Union{Symbol, FunCall, Constant}}
end

struct IndexCall
  name::Symbol
  typ::Type
  args::Vector{Symbol}
end

struct Lambda
  ring::Ring
  args::Vector{Symbol}
  domain::Vector{Union{FunCall, IndexCall}}
  value::Vector{Union{Symbol, FunCall, Constant}}
end

struct Funs
  main::Symbol
  funs::Dict{Symbol, Union{Lambda}}
end

struct ProcCall
  name::Symbol
  args::Vector{Symbol} # plus state pointer
end

struct Index
  name::Symbol
  typ::Type
  fun::Union{Symbol, Function}
  permutation::Vector{Int64}
end

struct SumProduct
  ring::Ring
  var::Symbol
  domain::Vector{Union{FunCall, IndexCall}}
  value::Vector{Union{ProcCall, Symbol}}
end

struct Proc
  name::Symbol
  args::Vector{Symbol}
  body::SumProduct
end

struct Procs
  main::Symbol
  indexes::Dict{Symbol, Index} # (env) -> state
  procs::Dict{Symbol, Proc} # (state, args...) -> ring_type
end

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

Base.eltype(ring::Ring{T}) where {T} = T

struct Var
  name::Symbol
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

# relation interface

struct Relation{T <: Tuple}
  columns::T
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

function can_index(::Type{Relation{T}}) where {T}
  true
end

function get_index(::Type{Relation{T}}, fun, permutation::Vector{Int64}) where {T}
  @assert permutation == collect(1:length(permutation)) "Can't permute $(index.permutation) yet"
  n = length(T.parameters)
  quote
    fun = $(esc(fun))
    RelationIndex(fun.columns, fill(1, $(n+1)), fill(length(fun.columns[1])+1, $(n+1)))
  end
end

function count(::Type{Relation{T}}, index, args::Vector{Symbol}) where {T}
  column = length(args)
  quote
    index = $(esc(index))
    index.his[$column+1] - index.los[$column+1]
  end
end

function iter(::Type{Relation{T}}, index, args::Vector{Symbol}, f) where {T}
  value = gensym("value")
  column = length(args)
  quote
    index = $(esc(index))
    while next(index, $(Val{column}))
      $(esc(value)) = index.columns[$column][index.los[$column+1]]
      $(esc(inline(f, value)))
    end
  end
end

function prepare(::Type{Relation{T}}, index, args::Vector{Symbol}) where {T}
  column = length(args)
  quote
    index = $(esc(index))
    index.his[$column+1] = index.los[$column]
  end
end

function contains(::Type{Relation{T}}, index, args::Vector{Symbol}) where {T}
  column = length(args)
  var = args[end]
  quote
    index = $(esc(index))
    seek(index, $(Val{column}), $(esc(var)))
  end
end

# function interface

function can_index(::Type{T}) where {T <: Function}
  false
end

function count(::Type{T}, fun, args::Vector{Symbol}) where {T <: Function}
  1
end

function iter(::Type{T}, fun, args::Vector{Symbol}, f) where {T <: Function}
  value = gensym("value")
  quote
    $(esc(value)) = $(esc(fun))($(map(esc, args[1:end-1])...))
    $(esc(inline(f, value)))
  end
end

function prepare(::Type{T}, fun, args::Vector{Symbol}) where {T <: Function}
  nothing
end

function contains(::Type{T}, fun, args::Vector{Symbol}) where {T <: Function}
  quote
    $(esc(fun))($(map(esc, args[1:end-1])...)) == $(esc(args[end]))
  end
end

# codegen

macro get_index(fun, index); get_index(index.typ, fun, index.permutation); end
macro count(call); count(call.typ, call.name, convert(Vector{Symbol}, call.args)); end
macro iter(call, f); iter(call.typ, call.name, convert(Vector{Symbol}, call.args), f); end
macro prepare(call); prepare(call.typ, call.name, convert(Vector{Symbol}, call.args)); end
macro contains(call); contains(call.typ, call.name, convert(Vector{Symbol}, call.args)); end

macro product(ring::Ring, domain::Vector{Union{FunCall, IndexCall}}, value::Vector{Union{ProcCall, Symbol}})
  code = :result
  for call in reverse(value)
    called = @match call begin
      _::ProcCall => :(($(call.name))($(call.args...)))
      _::Symbol => call
    end
    code = quote
      result = $(ring.mult)(result, $(esc(called)))
      if result == $(ring.zero)
        $(ring.zero)
      else
        $code
      end
    end
  end
  for call in reverse(domain)
    code = quote
      if !@contains($call)
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

macro sum(ring::Ring, call::Union{FunCall, IndexCall}, f)
  value = gensym("value")
  quote
    result = $(ring.zero)
    @iter($call, ($(esc(value))) -> begin
      result = $(ring.add)(result, $(esc(inline(f, value))))
    end)
    result
  end
end

macro run_proc(body::SumProduct)
  @assert !isempty(body.domain) "Cant join on an empty domain"
  quote
    $(@splice call in body.domain quote
      @prepare($call)
    end)
    mins = tuple($(@splice call in body.domain quote
      @count($call)
    end))
    min = Base.min(mins...)
    $(@splice i in 1:length(body.domain) quote
      if mins[$i] == min
        return @sum($(body.ring), $(body.domain[i]), ($(esc(body.var))) -> begin
          @product($(body.ring), $(body.domain[1:length(body.domain) .!= i]), $(body.value))
        end)
      end
      error("Impossibles!")
    end)
  end
end

macro define_procs(procs::Procs)
  name = gensym("lambda")
  child_name = gensym("join")
  quote
    $(@splice (_, proc) in procs.procs quote
      function $(esc(proc.name))($(map(esc, proc.args)...))
        @run_proc($(proc.body))
      end
    end)
    function $name(env)
      $(procs.main)($(@splice (_, index) in procs.indexes quote
        @get_index(env[$(Expr(:quote, index.fun))], $index)
      end))
    end
  end
end

function insert_indexes(lambda::Lambda, vars::Vector{Symbol}) ::Tuple{Vector{Union{FunCall, IndexCall}}, Dict{Symbol, Index}}
  domain = Union{FunCall, IndexCall}[]
  indexes = Dict{Symbol, Index}()
  for call in lambda.domain
    typ = fun_type(call.name)
    if can_index(typ)
      # sort args according to variable order
      n = length(call.args)
      permutation = Vector(1:n)
      sort!(permutation, by=(ix) -> findfirst(vars, call.args[ix]))
      name = gensym("index")
      indexes[name] = Index(name, typ, call.name, permutation)
      # insert all prefixes of args
      for i in 1:n
        push!(domain, IndexCall(name, typ, call.args[permutation][1:i]))
      end
    else
      push!(domain, FunCall(call.name, typ, call.args))
    end
  end
  (domain, indexes)
end

function Compiled.factorize(lambda::Lambda, vars::Vector{Symbol}) ::Procs
  domain, indexes = insert_indexes(lambda, vars)
  index_names = collect(keys(indexes))

  # for each call, figure out at which var we have all the args available
  latest_var_nums = map(domain) do call
    maximum(call.args) do arg
      findfirst(vars, arg)
    end
  end

  # make individual procs
  procs = map(1:length(vars)) do var_num
    var = vars[var_num]
    proc_domain = domain[latest_var_nums .== var_num]
    proc_value = (var in lambda.value) ? [var] : []
    body = SumProduct(lambda.ring, var, proc_domain, proc_value)
    args = vcat(index_names, vars[1:var_num-1])
    Proc(gensym("proc"), args, body)
  end

  # stitch them all together
  for i in 1:(length(procs)-1)
    push!(procs[i].body.value, ProcCall(procs[i+1].name, procs[i+1].args))
  end

  Procs(
    procs[1].name,
    indexes,
    Dict{Symbol, Proc}(proc.name => proc for proc in procs),
  )
end

function order_vars(lambda::Lambda) ::Vector{Symbol}
  # just use order vars appear in the ast for now
  union(map((call) -> call.args, lambda.domain)...)
end

function lower_constants(lambda::Lambda) ::Lambda
  constants = FunCall[]

  lower_constant = (arg) -> begin
    if isa(arg, Constant)
      var = gensym("constant")
      fun = @eval () -> $(arg.value)
      push!(constants, FunCall(fun, typeof(fun), [var]))
      var
    else
      arg
    end
  end

  domain = map(lambda.domain) do call
    typeof(call)(call.name, call.typ, map(lower_constant, call.args))
  end

  value = map(lower_constant, lambda.value)

  Lambda(lambda.ring, lambda.args, vcat(constants, domain), value)
end

function compile(lambda::Lambda, fun_type::Function, var_type::Function)
  lambda = lower_constants(lambda)
  vars = order_vars(lambda)
  procs = factorize(lambda, vars)
  code = macroexpand(quote
    @define_procs($procs)
  end)
  # @show simplify_expr(code)
  eval(code)
end

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast1 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:i, :x, :y, :z],
  [
    FunCall(:xx, Any, [:i, :x]),
    FunCall(:yy, Any, [:i, :y]),
    FunCall(:zz, Any, [:x, :y, :z]),
  ],
  [:z]
  )

polynomial_ast2 = Lambda(
  Ring{Int64}(+,*,1,0),
  [:x, :y, :z],
  [
    FunCall(:xx, Any, [:x, :x]),
    FunCall(:yy, Any, [:x, :y]),
    FunCall(:zz, Any, [:x, :y, :z]),
  ],
  [:z]
  )

zz(x, y) = (x * x) + (y * y) + (3 * x * y)

polynomial_ast3 = Lambda(
    Ring{Int64}(+,*,1,0),
    [:i, :x, :y, :t1, :t2, :t3, :z],
    [
      FunCall(:xx, Any, [:i, :x]),
      FunCall(:yy, Any, [:i, :y]),
      FunCall(*, Any, [:x, :x, :t1]),
      FunCall(*, Any, [:y, :y, :t2]),
      FunCall(*, Any, [Constant(3), :x, :y, :t3]),
      FunCall(+, Any, [:t1, :t2, :t3, :z])
    ],
    [:z]
    )
const xx = Relation((collect(0:3),collect(0:3)))
const yy = Relation((collect(0:3), collect(reverse(0:3))))
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
