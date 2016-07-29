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

function intersect(next, cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  @inbounds begin
    for ix in ixes
      ats[ix] = los[ix]
    end
    n = length(ixes)
    value = cols[ixes[n]][ats[ixes[n]]]
    fixed = 1
    while true 
      for ix in ixes
        if fixed == n
          for ix2 in ixes
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[ix2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          next()
        else 
          ats[ix] = gallop(cols[ix], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return 
        else 
          next_value = cols[ix][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end

srand(999)
edges_xy = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
# edges_xy = ([1, 2, 3, 3, 4], [2, 3, 1, 4, 2])
edges_yz = deepcopy(edges_xy)
edges_xz = (copy(edges_xy[2]), copy(edges_xy[1]))
quicksort!(edges_xy)
quicksort!(edges_yz)
quicksort!(edges_xz)

function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols = (edges_xy[1], edges_xy[2], Int64[], edges_yz[1], edges_yz[2], Int64[], edges_xz[1], edges_xz[2], Int64[])
  los = [1 for _ in 1:length(cols)]
  ats = [1 for _ in 1:length(cols)]
  his = [length(cols[i])+1 for i in 1:length(cols)]
  count = [0]
  
  cont4 = () -> count[1] += 1
  cont3 = () -> intersect(cont4, cols, los, ats, his, (5, 8))
  cont2 = () -> intersect(cont3, cols, los, ats, his, (2, 4))
  cont1 = () -> intersect(cont2, cols, los, ats, his, (1, 7))
  
  @time cont1()
  
  count[1]
end

f(edges_xy, edges_yz, edges_xz)
# @code_warntype f(edges_xy, edges_yz, edges_xz)

# readcsv(open("/home/jamie/soc-LiveJournal1.txt"))

end
