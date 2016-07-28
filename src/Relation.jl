module Relation

using Base.Cartesian

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
  
    @inline function swap3($(cs...), i, j, k)
      @inbounds begin
        $([quote
          $(tmps[c]) = $(cs[c])[k]
          $(cs[c])[k] = $(cs[c])[j]
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

    @inline function select_pivot!($(cs...), lo::Int, hi::Int)
      @inbounds begin
        mi = (lo+hi)>>>1
        if lt($(cs...), mi, lo)
          swap2($(cs...), lo, mi)
        end
        if lt($(cs...), hi, mi)
          if lt($(cs...), hi, lo)
            swap3($(cs...), lo, mi, hi)
          else
            swap2($(cs...), mi, hi)
          end
        end
        swap2($(cs...), lo, mi)
      end
      return lo
    end

    function partition!($(cs...), lo::Int, hi::Int)
      pivot = select_pivot!($(cs...), lo, hi)
      i, j = lo, hi
      @inbounds while true
        i += 1; j -= 1
        while lt($(cs...), i, pivot); i += 1; end;
        while lt($(cs...), pivot, j); j -= 1; end;
        i >= j && break
        swap2($(cs...), i, j)
      end
      swap2($(cs...), pivot, j)
      return j
    end

    function quicksort!($(cs...), lo::Int, hi::Int)
      @inbounds while lo < hi
        if hi-lo <= 20 
          insertion_sort!($(cs...), lo, hi)
          return 
        end
        j = partition!($(cs...), lo, hi)
        if j-lo < hi-j
          lo < (j-1) && quicksort!($(cs...), lo, j-1)
          lo = j+1
        else
          j+1 < hi && quicksort!($(cs...), j+1, hi)
          hi = j-1
        end
      end
      return
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

# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::Vector{T}, value::T, lo::Int64, hi::Int64, cmp) 
  @inbounds if (lo < hi) && cmp(column[lo], value)
    step = 1
    while (lo + step < hi) && cmp(column[lo + step], value)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && cmp(column[lo + step], value)
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  lo 
end 

@generated function intersect{T,N}(cols::NTuple{N, Vector{T}}, los::NTuple{N, Int64}, his::NTuple{N, Int64}, handler)
  # assume los/his are valid 
  # los inclusive, his exclusive
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      local value::$T
      @nextract $N col cols
      @nextract $N lo los 
      @nextract $N hi his 
      value = col_1[lo_1]
      inited = false
      while true 
        @nexprs $N c->
        begin
          if inited && (col_c[lo_c] == value)
            @nexprs $N c2-> matching_hi_c2 = gallop(col_c2, value, lo_c2, hi_c2, <=)
            handler(value, (@ntuple $N lo), (@ntuple $N matching_hi))
            lo_c = matching_hi_c
            # TODO can we set los = matching_his without breaking the stop condition?
          else 
            lo_c = gallop(col_c, value, lo_c, hi_c, <)
          end
          if lo_c >= hi_c
            return 
          else 
            value = col_c[lo_c]
          end
          inited = true
        end
      end
    end
  end
end

# TODO count stationary, reset to 0 on jump

intersect(([1,2,3,4,5],[2,5,7,9]), (1,1), (6, 5), println)

srand(999)
edges_x = ([i for i in 1:100000], shuffle([i for i in 1:100000]))
edges_y = deepcopy(edges_x)
edges_z = (copy(edges_x[2]), copy(edges_x[1]))
quicksort!(edges_x)
quicksort!(edges_y)
quicksort!(edges_z)

function f(edges_x, edges_y, edges_z) 
  n = length(edges_x[1])
  a = 0
  b = 0
  c = 0
  @time intersect((edges_x[1], edges_z[1]), (1,1), (n,n), @inline function (x, x_los, x_his)
    a += 1
    intersect((edges_x[2], edges_y[1]), (x_los[1],1), (x_his[1],n), @inline function (y, y_los, y_his)
      b += 1
      intersect((edges_y[2], edges_z[2]), (y_los[2], x_los[2]), (y_his[2], x_his[2]), @inline function (z, z_los, z_his)
        c += 1
      end)
    end)
  end)
  (a + b + c)
end

# @code_warntype 
  
  f(edges_x::Tuple{Vector{Int64}, Vector{Int64}}, 
  edges_y::Tuple{Vector{Int64}, Vector{Int64}}, 
  edges_z::Tuple{Vector{Int64}, Vector{Int64}})
# @code_llvm gallop(edges_x[1], 3, 1, 3, <)

# readcsv(open("/home/jamie/soc-LiveJournal1.txt"))

end
