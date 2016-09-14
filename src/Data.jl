module Data

using Base.Test
using BenchmarkTools

@generated function cmp_in{T <: Tuple}(xs::T, ys::T, x_at::Int64, y_at::Int64)
  n = length(T.parameters)
  if n == 0
    :(return 0)
  else 
    quote
      $(Expr(:meta, :inline))
      @inbounds begin 
        $([:(result = cmp(xs[$c][x_at], ys[$c][y_at]); if result != 0; return result; end) for c in 1:(n-1)]...)
        return cmp(xs[$n][x_at], ys[$n][y_at])
      end
    end
  end
end

@generated function swap_in{T <: Tuple}(xs::T, i::Int64, j::Int64)
  n = length(T.parameters)
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      $([quote 
        let tmp = xs[$c][i]
          xs[$c][i] = xs[$c][j]
          xs[$c][j] = tmp
        end
      end for c in 1:n]...)
    end
  end
end

@generated function push_in!{T <: Tuple}(result::T, cs::T, i::Int64)
  n = length(T.parameters)
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      $([:(push!(result[$c], cs[$c][i])) for c in 1:n]...)
    end
  end
end

# sorting cribbed from Base.Sort
# but unrolls `cmp` and `swap` to avoid heap allocation of rows
# and use random pivot selection because stdlib pivot caused 1000x sadness on real data

function insertion_sort!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds for i = lo+1:hi
    j = i
    while j > lo && (cmp_in(cs, cs, j, j-1) == -1)
      swap_in(cs, j, j-1)
      j -= 1
    end
  end
end

function partition!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds begin
    pivot = rand(lo:hi)
    swap_in(cs, pivot, lo)
    i, j = lo+1, hi
    while true
      while (i <= j) && (cmp_in(cs, cs, i, lo) == -1); i += 1; end;
      while (i <= j) && (cmp_in(cs, cs, lo, j) == -1); j -= 1; end;
      i >= j && break
      swap_in(cs, i, j)
      i += 1; j -= 1
    end
    swap_in(cs, lo, j)
    return j
  end
end

function quicksort!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds if hi-lo <= 0
    return
  elseif hi-lo <= 20 
    insertion_sort!(cs, lo, hi)
  else
    j = partition!(cs, lo, hi)
    quicksort!(cs, lo, j-1)
    quicksort!(cs, j+1, hi)
  end
end

function quicksort!{T <: Tuple}(cs::T)
  quicksort!(cs, 1, length(cs[1]))
end

function merge_sorted!{T <: Tuple, K <: Tuple}(old::T, new::T, old_key::K, new_key::K, result::T)
  @inbounds begin
    old_at = 1
    new_at = 1
    old_hi = length(old[1])
    new_hi = length(new[1])
    while old_at <= old_hi && new_at <= new_hi
      c = cmp_in(old_key, new_key, old_at, new_at)
      if c == 0
        push_in!(result, new, new_at)
        old_at += 1
        new_at += 1
      elseif c == 1
        push_in!(result, new, new_at)
        new_at += 1
      else 
        push_in!(result, old, old_at)
        old_at += 1
      end
    end
    while old_at <= old_hi
      push_in!(result, old, old_at)
      old_at += 1
    end
    while new_at <= new_hi
      push_in!(result, new, new_at)
      new_at += 1
    end
  end
end

function assert_no_dupes_sorted{K <: Tuple}(key::K)
  for at in 2:length(key[1])
    @assert cmp_in(key, key, at, at-1) == 1
  end
end

type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
  key_types::Vector{Type}
  val_types::Vector{Type}
end

function is_type_expr(expr)
  isa(expr, Symbol) || (isa(expr, Expr) && expr.head == :(.))
end

# examples:
# @relation height_at(Int64, Int64) = Float64
# @relation married(String, String)
# @relation state() = (Int64, Symbol)
macro relation(expr) 
  if expr.head == :(=)
    name_and_keys = expr.args[1]
    vals_expr = expr.args[2]
  else 
    name_and_keys = expr
    vals_expr = Expr(:tuple)
  end
  @assert name_and_keys.head == :call
  name = name_and_keys.args[1]
  @assert isa(name, Symbol)
  keys = name_and_keys.args[2:end]
  for key in keys 
    @assert is_type_expr(key)
  end
  if vals_expr.head == :block
    vals_expr = vals_expr.args[2]
  end
  if is_type_expr(vals_expr)
    vals = [vals_expr]
  else 
    @assert vals_expr.head == :tuple
    vals = vals_expr.args
  end
  for val in vals 
    @assert is_type_expr(val)
  end
  typs = [keys..., vals...]
  quote 
    columns = tuple($([:(Vector{$(esc(typ))}()) for typ in typs]...))
    indexes = Dict{Vector{Int64}, typeof(columns)}()
    const $(esc(name)) = Relation(columns, indexes, Type[$(map(esc, keys)...)], Type[$(map(esc, vals)...)])
  end
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  get!(relation.indexes, order) do
    index::T = tuple([(ix in order) ? copy(column) : Vector{eltype(column)}() for (ix, column) in enumerate(relation.columns)]...)
    quicksort!(tuple([index[ix] for ix in order]...))
    index
  end
end

function Base.push!{T}(relation::Relation{T}, values)
  @assert length(relation.columns) == length(values)
  for ix in 1:length(values)
    push!(relation.columns[ix], values[ix])
  end
  empty!(relation.indexes)
  # TODO can preserve indexes when inserted value is at end or beginning
  # TODO remove dupes
end

function Base.empty!{T}(relation::Relation{T})
  for column in relations.columns
    empty!(column)
  end
  empty!(relation.indexes)
end

function Base.length(relation::Relation)
  if length(relation.columns) == 0
    return 0
  else 
    length(relation.columns[1])
  end
end

function Base.merge{T}(old::Relation{T}, new::Relation{T})
  # TODO should Relation{T} be typed Relation{K,V} instead?
  # assert(old.key_types == new.key_types)
  # assert(old.val_types == new.val_types)
  order = collect(1:(length(old.key_types) + length(old.val_types)))
  old_index = index(old, order)
  new_index = index(new, order)
  num_keys = length(old.key_types)
  result_columns = tuple([Vector{eltype(column)}() for column in old.columns]...)
  merge_sorted!(old_index, new_index, old_index[1:num_keys], new_index[1:num_keys], result_columns)
  result_indexes = Dict{Vector{Int64}, typeof(result_columns)}(order => result_columns)
  Relation(result_columns, result_indexes, old.key_types, old.val_types)
end

function replace!{T}(old::Relation{T}, new::Relation{T})
  old.columns = new.columns
  old.indexes = copy(new.indexes)
end

function Base.merge!{T}(old::Relation{T}, new::Relation{T})
  replace!(old, merge(old, new))
end

function Relation{T}(columns::T)
  deduped = tuple((Vector{eltype(column)}() for column in columns)...)
  quicksort!(columns)
  at = 1
  hi = length(columns[1])
  while at <= hi
    push_in!(deduped, columns, at)
    while (at += 1; (at <= hi) && cmp_in(columns, columns, at, at-1) == 0) end
  end
  order = collect(1:length(columns))
  key_types = Type[eltype(column) for column in columns]
  Relation(deduped, Dict{Vector{Int64},T}(order => deduped), key_types, Type[])
end

function assert_no_dupes{T}(relation::Relation{T})
  order = collect(1:(length(relation.key_types) + length(relation.val_types)))
  key = index(relation, order)[1:length(relation.key_types)]
  assert_no_dupes_sorted(key)
  relation
end

import Atom
function Atom.render(editor::Atom.Editor, relation::Relation)
  Atom.render(editor, relation.columns)
end

export Relation, @relation, index, assert_no_dupes, replace!

function test()
  for i in 1:10000
    srand(i)
    x = rand(Int64, i)
    sx = sort(copy(x))
    quicksort!((x,))
    @test x == sx
  end
  
  for i in 1:10000
    srand(i)
    x = unique(rand(1:i, i))
    y = rand(1:i, length(x))
    z = rand(1:i, length(x))
    a = Relation((x,y), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
    b = Relation((x,z), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
    c = merge(a,b)
    @test index(c, [1,2]) == index(b, [1,2])
  end
  
  for i in 1:10000
    srand(i)
    x = unique(rand(1:i, i))
    x1 = [i*2 for i in x]
    x2 = [i*2+1 for i in x]
    y = rand(1:i, length(x))
    z = rand(1:i, length(x))
    a = Relation((x1,y), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
    b = Relation((x2,z), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
    c = merge(a,b)
    @test length(c) == length(x1) + length(x2)
  end
end

function bench()
  srand(999)
  x = rand(Int64, 10000)
  @show @benchmark quicksort!((copy($x),))
  
  srand(999)
  y = [string(i) for i in rand(Int64, 10000)]
  @show @benchmark quicksort!((copy($y),))
  
  srand(999)
  x = unique(rand(1:10000, 10000))
  y = rand(1:10000, length(x))
  z = rand(1:10000, length(x))
  a = Relation((x,y), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
  b = Relation((x,z), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
  @show @benchmark merge($a,$b)
end

end
