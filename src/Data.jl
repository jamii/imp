module Data

using Match
using NullableArrays
using Base.Test
using BenchmarkTools

@generated function cmp_in{T <: Tuple}(xs::T, ys::T, x_at::Int, y_at::Int)
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

@generated function swap_in{T <: Tuple}(xs::T, i::Int, j::Int)
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

@generated function push_in!{T <: Tuple}(result::T, cs::T, i::Int)
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

# TODO should be typed {K,V} instead of {T}, but pain in the ass to change now
mutable struct Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  num_keys::Int
  indexes::Dict{Vector{Int},T}
end

function index{T}(relation::Relation{T}, order::Vector{Int})
  get!(relation.indexes, order) do
    columns = tuple(((ix in order) ? copy(column) : empty(column) for (ix, column) in enumerate(relation.columns))...)
    quicksort!(tuple((columns[ix] for ix in order)...))
    columns
  end::T
end

function dedup_sorted!{T}(columns::T, key, val, deduped::T)
  at = 1
  hi = length(columns[1])
  while at <= hi
    push_in!(deduped, columns, at)
    while (at += 1; (at <= hi) && cmp_in(key, key, at, at-1) == 0) # skip dupe keys
      @assert cmp_in(val, val, at, at-1) == 0 # no key collisions allowed
    end
  end
end

function is_unique_and_sorted{T}(columns::T)
  for i in 2:length(columns[1])
    if cmp_in(columns, columns, i, i-1) != 1
      return false
    end
  end
  return true
end

function Relation(columns, num_keys::Int)
  order = collect(1:length(columns))
  if is_unique_and_sorted(columns)
    Relation(columns, num_keys, Dict{Vector{Int}, typeof(columns)}(order => columns))
  else
    quicksort!(columns)
    deduped::typeof(columns) = map((column) -> empty(column), columns)
    key = columns[1:num_keys]
    val = columns[num_keys+1:1]
    dedup_sorted!(columns, key, val, deduped)
    Relation(deduped, num_keys, Dict{Vector{Int}, typeof(deduped)}(order => deduped))
  end
end

function parse_relation(expr)
  (head, tail) = @match expr begin
    Expr(:call, [:(=>), head, tail], _) => (head, tail)
    head => (head, :(()))
  end
  (name, keys) = @match head begin
    Expr(:call, [name, keys...], _) => (name, keys)
    Expr(:tuple, keys, _) => ((), keys)
    _ => error("Can't parse $expr as relation")
  end
  vals = @match tail begin 
    Expr(:tuple, vals, _) => vals
    _ => [tail]
  end
  (name, keys, vals)
end

function column_type{T}(_::Type{T})
  Vector{T}
end

# TODO not useful until stack allocation of structs is improved
# function column_type{T}(_::Type{Nullable{T}})
#   NullableVector{T}
# end

# examples:
# @relation (Int, Float64)
# @relation (Int,) => Int
# @relation height_at(Int) => Float64
# @relation married(String, String)
# @relation state() => (Int, Symbol)
macro relation(expr) 
  (name, keys, vals) = parse_relation(expr)
  typs = [keys..., vals...]
  order = collect(1:length(typs))
  body = quote 
    columns = tuple($([:(column_type($(esc(typ)))()) for typ in typs]...))
    Relation(columns, $(length(keys)))
  end
  if name != ()
    :(const $(esc(name)) = $body)
  else
    body
  end
end

function foreach_diff{T <: Tuple, K <: Tuple}(old::T, new::T, old_key::K, new_key::K, old_only, new_only, old_and_new)
  @inbounds begin
    old_at = 1
    new_at = 1
    old_hi = length(old[1])
    new_hi = length(new[1])
    while old_at <= old_hi && new_at <= new_hi
      c = cmp_in(old_key, new_key, old_at, new_at)
      if c == 0
        old_and_new(old, new, old_at, new_at)
        old_at += 1
        new_at += 1
      elseif c == 1
        new_only(new, new_at)
        new_at += 1
      else 
        old_only(old, old_at)
        old_at += 1
      end
    end
    while old_at <= old_hi
      old_only(old, old_at)
      old_at += 1
    end
    while new_at <= new_hi
      new_only(new, new_at)
      new_at += 1
    end
  end
end

function diff{T}(old::Relation{T}, new::Relation{T})
  @assert old.num_keys == new.num_keys 
  order = collect(1:length(old.columns))
  old_index = index(old, order)
  new_index = index(new, order)
  old_only_columns = tuple([empty(column) for column in old.columns]...)
  new_only_columns = tuple([empty(column) for column in new.columns]...)
  foreach_diff(old_index, new_index, old_index, new_index, 
    (o, i) -> push_in!(old_only_columns, o, i),
    (n, i) -> push_in!(new_only_columns, n, i),
    (o, n, oi, ni) -> ())
  (old_only_columns, new_only_columns)
end

function diff_ixes{T}(old::Relation{T}, new::Relation{T})::Tuple{Vector{Int64}, Vector{Int64}}
  @assert old.num_keys == new.num_keys 
  order = collect(1:length(old.columns))
  old_index = index(old, order)
  new_index = index(new, order)
  old_only_ixes = Vector{Int64}()
  new_only_ixes = Vector{Int64}()
  foreach_diff(old_index, new_index, old_index, new_index, 
    (o, i) -> push!(old_only_ixes, i),
    (n, i) -> push!(new_only_ixes, i),
    (o, n, oi, ni) -> ())
  (old_only_ixes, new_only_ixes)
end

function Base.merge{T}(old::Relation{T}, new::Relation{T})
  if old.num_keys != new.num_keys 
    error("Mismatch in num_keys - $(old.num_keys) vs $(new.num_keys) in merge($old, $new)")
  end
  if length(old.columns[1]) == 0
    return new
  end
  if length(new.columns[1]) == 0
    return old
  end
  order = collect(1:length(old.columns))
  old_index = old.indexes[order]
  new_index = new.indexes[order]
  result_columns::T = tuple((empty(column) for column in old.columns)...)
  foreach_diff(old_index, new_index, old_index[1:old.num_keys], new_index[1:new.num_keys], 
    (o, i) -> push_in!(result_columns, o, i),
    (n, i) -> push_in!(result_columns, n, i),
    (o, n, oi, ni) -> push_in!(result_columns, n, ni))
  result_indexes = Dict{Vector{Int}, Tuple}(order => result_columns)
  Relation{T}(result_columns, old.num_keys, result_indexes)
end

function replace!{T}(old::Relation{T}, new::Relation{T})
  old.columns = new.columns
  old.indexes = copy(new.indexes) # shallow copy of Dict
  old
end

function Base.merge!{T}(old::Relation{T}, new::Relation{T})
  replace!(old, merge(old, new))
end

function Base.push!{T}(relation::Relation{T}, values)
  merge!(relation, Relation(map((i) -> eltype(relation.columns[i])[values[i]], tuple(1:length(values)...)), relation.num_keys))
end

@inline function Base.length(relation::Relation)
  length(relation.columns)
end

@inline function Base.getindex(relation::Relation, ix)
  relation.columns[ix]
end

function empty(coll)
  typeof(coll)()
end

function empty(relation::Relation) 
  Relation(map((c) -> empty(c), relation.columns), relation.num_keys, empty(relation.indexes))
end

function Base.copy(relation::Relation)
  Relation(relation.columns, relation.num_keys, copy(relation.indexes))
end

export Relation, @relation, index, parse_relation, empty, diff

function test()
  for i in 1:10000
    srand(i)
    x = rand(Int, i)
    sx = sort(copy(x))
    quicksort!((x,))
    @test x == sx
  end
  
  for i in 1:10000
    srand(i)
    x = unique(rand(1:i, i))
    y = rand(1:i, length(x))
    z = rand(1:i, length(x))
    a = Relation((x,y), 1)
    b = Relation((x,z), 1)
    c = merge(a,b)
    @test c.columns == b.columns
  end
  
  for i in 1:10000
    srand(i)
    x = unique(rand(1:i, i))
    x1 = [i*2 for i in x]
    x2 = [i*2+1 for i in x]
    y = rand(1:i, length(x))
    z = rand(1:i, length(x))
    a = Relation((x1,y), 1)
    b = Relation((x2,z), 1)
    c = merge(a,b)
    @test length(c.columns[1]) == length(x1) + length(x2)
  end
  
  @test Base.return_types(Relation, (Tuple{Vector{Int}, Vector{String}}, Int)) == [Relation{Tuple{Vector{Int}, Vector{String}}}]
end

function bench()
  srand(999)
  x = rand(Int, 10000)
  @show @benchmark quicksort!((copy($x),))
  
  srand(999)
  y = [string(i) for i in rand(Int, 10000)]
  @show @benchmark quicksort!((copy($y),))
  
  srand(999)
  x = unique(rand(1:10000, 10000))
  y = rand(1:10000, length(x))
  z = rand(1:10000, length(x))
  a = Relation((x,y), 1)
  b = Relation((x,z), 1)
  @show @benchmark merge($a,$b)
end

end
