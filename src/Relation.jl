module Relation

abstract Columns{T} <: AbstractVector{T}

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
  
  # sorting cribbed from Base.sort
  
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

for i in 1:10
  eval(define_columns(i))
end

# n = Int64(1E6)
# make_ids() = rand(1:n, n)
# 
# ids = make_ids()
# @time sort!(ids, alg=QuickSort)
# 
# eval(:(while true
#     break
#   end))
# 
# c2 = (make_ids(), [1 for _ in ids])
# @time quicksort!(c2)
# println(c2[1][1:100])
# as = copy(c2[1])
# sort!(as, alg=QuickSort)
# println(as[1:100])
# for i in 1:length(c2[1])
#   if (as[i] != c2[1][i])
#     println("at ", i, " ", as[i], " ", c2[1][i])
#     break
#   end
# end
# assert(c2[1] == as)

end
