module Hashed

import Data

# equivalent to `hash(tuple(xs[1][at], xs[2][at]...))`
@generated function hash_in{T <: Tuple}(xs::T, at::Integer)
  n = length(T.parameters)
  body = :(zero(UInt))
  for i in 1:n
    body = :(hash(xs[$i][at], $body))
  end
  body
end

function distance(size, slot, h)
  (Int(slot) - Int(h) + size) % size
end
  
function set_range(hashtable, size, key, range)
  h = (hash_in(key, range.start) % size) + 1
  slot = h
  while true
    range2 = hashtable[slot]
    if range2 == Int32(0):Int32(-1)
      hashtable[slot] = range
      return
    else
      h2 = (hash_in(key, range2.start) % size) + 1
      if distance(size, slot, h2) < distance(size, slot, h)
        # steal from the rich!
        hashtable[slot] = range
        h = h2
        range = range2
      end
    end
    slot = (slot % size) + 1
  end
end

function hashtable(key)
  ranges = UnitRange{Int32}[]
  lo = 1
  len = length(key[1])
  while lo <= len
    hi = lo
    while (hi+1 <= len) && Data.cmp_in(key, key, lo, hi+1) == 0
      hi += 1
    end
    push!(ranges, lo:hi)
    lo = hi + 1
  end
  
  size = Int(ceil(length(ranges) * 1.5))
  hashtable = [Int32(0):Int32(-1) for _ in 1:size]
  for range in ranges
    set_range(hashtable, size, key, range)
  end
  
  return hashtable 
end

function probe_range(key, hashtable)
  size = length(hashtable)
  min_probe_range::Int32 = typemax(Int32)
  max_probe_range::Int32 = 0
  for (slot, range) in enumerate(hashtable)
    if range != Int32(0):Int32(-1)
      h = (hash_in(key, range.start) % size) + 1
      min_probe_range = min(min_probe_range, distance(size, slot, h))
      max_probe_range = max(max_probe_range, distance(size, slot, h))
    end
  end
  if min_probe_range == typemax(Int32)
    Int32(0):Int32(-1)
  else
    min_probe_range:max_probe_range
  end
end
  
type Index{C}
  columns::C
  hashtables::Vector{Vector{UnitRange{Int32}}}
  probe_ranges::Vector{UnitRange{Int32}}
end

function Index(columns)
  Data.quicksort!(columns)
  hashtables = [hashtable(columns[1:i]) for i in 1:length(columns)]
  probe_ranges = [probe_range(columns[1:i], hashtables[i]) for i in 1:length(columns)]
  Index(columns, hashtables, probe_ranges)
end

type Relation{T}
  unique::Index
  indexes::Dict{Vector{Int}, Index}
  num_keys::Int
end

function Relation(columns, num_keys::Int)
  T = Tuple{map(eltype, columns)...}
  Data.quicksort!(columns)
  deduped::typeof(columns) = map((column) -> Vector{eltype(column)}(), columns)
  key = columns[1:num_keys]
  val = columns[num_keys+1:1]
  Data.dedup_sorted!(columns, key, val, deduped)
  unique = Index(deduped)
  order = collect(1:length(columns))
  Relation{T}(unique, Dict{Vector{Int}, Index}(order => unique), num_keys)
end

function index(relation::Relation, order::Vector{Int64})
  get!(relation.indexes, order) do
    columns = tuple([copy(relation.unique.columns[ix]) for ix in order]...)
    Index(columns)
  end
end

@generated function index{T, O}(relation::Relation{T}, ::Type{Val{O}})
  order = collect(O)
  typs = [Vector{T.parameters[ix]} for ix in order]
  quote
    index(relation, $order)::Index{Tuple{$(typs...)}}
  end
end

function get_range(column, hashtable, probe_range, range, hash, key)
  size = length(hashtable)
  h = (hash % size) + 1
  slot = h + probe_range.start
  while distance(size, slot, h) <= probe_range.stop
    range2 = hashtable[slot]
    if (range2 == Int32(0):Int32(-1))
      return Int32(0):Int32(-1)
    elseif (range2.start >= range.start) && 
      (range2.stop <= range.stop) && 
      (column[range2.start] == key)
      return range2
    end
    slot = (slot % size) + 1
  end
  return Int32(0):Int32(-1)
end

type Finger{T}
  column::Vector{T}
  hashtable::Vector{UnitRange{Int32}}
  probe_range::UnitRange{Int32}
  hash::UInt
  range::UnitRange{Int32}
end

@inline function finger(relation::Relation, index::Index)
  Finger(Void[], UnitRange{Int32}[], Int32(0):Int32(-1), UInt(0), Int32(1):Int32(length(index.columns[1])))
end

@inline function finger{col_ix}(relation::Relation, index, finger, ::Type{Val{col_ix}})
  Finger(index.columns[col_ix], index.hashtables[col_ix], index.probe_ranges[col_ix], UInt(0), Int32(0):Int32(-1))
end
  
@inline function Base.length(finger::Finger)
  # not technically correct - may be repeated values
  length(finger.range)
end

@inline function project{T,T2}(finger::Finger{T}, down_finger::Finger{T2}, val)
  down_finger.hash = hash(val, finger.hash)
  down_finger.range = get_range(down_finger.column, down_finger.hashtable, down_finger.probe_range, finger.range, down_finger.hash, val)
  length(down_finger.range) > 0
end

@inline function Base.start{T,T2}(finger::Finger{T}, down_finger::Finger{T2})
  if length(finger.range) == 0
    false 
  else 
    val = down_finger.column[finger.range.start]
    down_finger.hash = hash(val, finger.hash)
    down_finger.range = get_range(down_finger.column, down_finger.hashtable, down_finger.probe_range, finger.range, down_finger.hash, val)
    true
  end
end

@inline function Base.next{T,T2}(finger::Finger{T}, down_finger::Finger{T2})
  if down_finger.range.stop >= finger.range.stop
    false
  else
    val = down_finger.column[down_finger.range.stop+1]
    down_finger.hash = hash(val, finger.hash)
    down_finger.range = get_range(down_finger.column, down_finger.hashtable, down_finger.probe_range, finger.range, down_finger.hash, val)
    true
  end
end

@inline function head{C, C2}(finger::Finger{C}, down_finger::Finger{C2})
  down_finger.column[down_finger.range.start]
end

function sorted_columns(relation)
  relation.unique.columns
end

function diff(old, new)
  Data.diff(Data.Relation(old.unique.columns, old.num_keys), Data.Relation(new.unique.columns, new.num_keys))
end

export Relation, Index, index, Finger, finger, project, head, sorted_columns, diff

end
