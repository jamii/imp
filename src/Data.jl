module Data

function define_columns(n)
  cs = [symbol("c", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  tmps = [symbol("tmp", c) for c in 1:n]
  
  quote
    
    @inline function lt($(cs...), i, j) 
      @inbounds begin 
        $([:(if !isequal($(cs[c])[i], $(cs[c])[j]); return isless($(cs[c])[i], $(cs[c])[j]); end) for c in 1:(n-1)]...)
        return isless($(cs[n])[i], $(cs[n])[j])
      end
    end
    
    @inline function lt2($(cs...), $(tmps...), j) 
      @inbounds begin 
        $([:(if !isequal($(tmps[c]), $(cs[c])[j]); return isless($(tmps[c]), $(cs[c])[j]); end) for c in 1:(n-1)]...)
        return isless($(tmps[n]), $(cs[n])[j])
      end
    end
    
    @inline function swap2($(cs...), i, j)
      @inbounds begin
        $([quote
          $(tmps[c]) = $(cs[c])[j]
          $(cs[c])[j] = $(cs[c])[i]
          $(cs[c])[i] = $(tmps[c])
        end for c in 1:n]...)
      end
    end

    # sorting cribbed from Base.Sort

    function insertion_sort!($(cs...), lo::Int, hi::Int)
      @inbounds for i = lo+1:hi
        j = i
        $([:($(tmps[c]) = $(cs[c])[i]) for c in 1:n]...)
        while j > lo
          if lt2($(cs...), $(tmps...), j-1)
            $([:($(cs[c])[j] = $(cs[c])[j-1]) for c in 1:n]...)
            j -= 1
            continue
          end
          break
        end
        $([:($(cs[c])[j] = $(tmps[c])) for c in 1:n]...)
      end
    end

    function partition!($(cs...), lo::Int, hi::Int)
      @inbounds begin
        pivot = rand(lo:hi)
        swap2($(cs...), pivot, lo)
        i, j = lo+1, hi
        while true
          while (i <= j) && lt($(cs...), i, lo); i += 1; end;
          while (i <= j) && lt($(cs...), lo, j); j -= 1; end;
          i >= j && break
          swap2($(cs...), i, j)
          i += 1; j -= 1
        end
        swap2($(cs...), lo, j)
        return j
      end
    end

    function quicksort!($(cs...), lo::Int, hi::Int)
      @inbounds if hi-lo <= 0
        return
      elseif hi-lo <= 20 
        insertion_sort!($(cs...), lo, hi)
      else
        j = partition!($(cs...), lo, hi)
        quicksort!($(cs...), lo, j-1)
        quicksort!($(cs...), j+1, hi)
      end
    end

    function quicksort!{$(ts...)}(cs::Tuple{$(ts...)})
      quicksort!($([:(cs[$c]) for c in 1:n]...), 1, length(cs[1]))
      return cs
    end

  end

end

for n in 1:10
  eval(define_columns(n))
end

type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
  key_types::Vector{Type}
  val_types::Vector{Type}
end

function Relation{T}(columns::T)
  Relation(columns, Dict{Vector{Int64},T}(), Type[eltype(column) for column in columns], Type[])
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
  assert(name_and_keys.head == :call)
  name = name_and_keys.args[1]
  assert(isa(name, Symbol))
  keys = name_and_keys.args[2:end]
  for key in keys 
    assert(isa(key, Symbol))
  end
  if vals_expr.head == :block
    vals_expr = vals_expr.args[2]
  end
  if isa(vals_expr, Symbol) 
    vals = [vals_expr]
  else 
    assert(vals_expr.head == :tuple)
    vals = vals_expr.args
  end
  for val in vals 
    assert(isa(val, Symbol))
  end
  typs = [keys..., vals...]
  quote 
    columns = tuple($([:(Vector{$typ}()) for typ in typs]...))
    indexes = Dict{Vector{Int64}, typeof(columns)}()
    $(esc(name)) = Relation(columns, indexes, Type[$(keys...)], Type[$(vals...)])
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
  assert(length(relation.columns) == length(values))
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

import Atom
function Atom.render(editor::Atom.Editor, relation::Relation)
  Atom.render(editor, relation.columns)
end

export Relation, index

# for i in 1:10000
#   srand(i)
#   x = rand(Int64, i)
#   assert(quicksort!((copy(x),))[1] == sort(copy(x)))
# end

end
