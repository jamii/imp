module Relation

function define_columns(n)
  cs = [symbol("c", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  tmps = [symbol("tmp", c) for c in 1:n]
  
  :(begin
  
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
      $([:(begin
      $(tmps[c]) = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
  end
  end

  @inline function swap3($(cs...), i, j, k)
    @inbounds begin
      $([:(begin
      $(tmps[c]) = $(cs[c])[k]
      $(cs[c])[k] = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
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
  
end)
  
end

for n in 1:10
  eval(define_columns(n))
end

# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::Vector{T}, value::T, lo::Int64, hi::Int64, cmp) 
  if (lo < hi) && cmp(column[lo], value)
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

@inline function intersect{T,N}(handler, cols::NTuple{N, Vector{T}}, los::Vector{Int64}, his::Vector{Int64})
  # assume los/his are valid 
  # los inclusive, his exclusive
  n = length(cols)
  local value::T
  value = cols[n][los[n]]
  inited = false
  while true 
    for c in 1:n 
      if inited && (cols[c][los[c]] == value)
        matching_his = [gallop(cols[c], value, los[c], his[c], <=) for c in 1:n]
        handler(value, los, matching_his)
        los[c] = matching_his[c]
        # TODO can we set los = matching_his without breaking the stop condition?
      else 
        los[c] = gallop(cols[c], value, los[c], his[c], <)
      end
      if los[c] >= his[c]
        return 
      else 
        value = cols[c][los[c]]
      end
    end
    inited = true
  end
end

intersect(println, [[1,2,3,4,5],[2,5,7,9]], [1,1], [6, 5])

edges_x = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
edges_y = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
edges_z = [[1, 2, 2, 3, 4], [3, 1, 4, 2, 3]]

function f() 
  intersect([edges_x[1], edges_z[1]], [1,1], [6,6]) do x, x_los, x_his
    intersect([edges_x[2], edges_y[1]], [x_los[1],1], [x_his[1],6]) do y, y_los, y_his
      intersect([edges_y[2], edges_z[2]], [y_los[2], x_los[2]], [y_his[2], x_his[2]]) do z, z_los, z_his
        println(x,y,z)
      end
    end
  end
end

end
