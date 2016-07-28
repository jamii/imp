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

x = quote (a,2,3) end
fieldnames(x)
y = x.args[2]
y.head
typeof(y.args[1])

function unpack(expr)
  assert(expr.head == :tuple)
  for value in expr.args
    assert(typeof(value) == Symbol)
  end
  expr.args
end

macro intersect(cols, los, his, next_los, next_his, handler)
  cols = unpack(cols)
  los = unpack(los)
  his = unpack(his)
  next_los = unpack(next_los)
  next_his = unpack(next_his)
  n = length(cols)
  quote
    # assume los/his are valid 
    # los inclusive, his exclusive
    @inbounds begin 
      value = $(esc(cols[n]))[$(esc(los[n]))]
      inited = false
      finished = false
      while !finished
        $([
        quote
          if inited && ($(esc(cols[c]))[$(esc(los[c]))] == value)
            $([
            quote
              $(esc(next_los[c2])) = $(esc(los[c2]))
              $(esc(next_his[c2])) = gallop($(esc(cols[c2])), value, $(esc(los[c2])), $(esc(his[c2])), <=)
            end
            for c2 in 1:n]...)
            $handler # TODO huge code duplication
            $(esc(los[c])) = $(esc(next_his[c]))
          else 
            $(esc(los[c])) = gallop($(esc(cols[c])), value, $(esc(los[c])), $(esc(his[c])), <)
          end
          if $(esc(los[c])) >= $(esc(his[c]))
            finished = true
          else 
            value = $(esc(cols[c]))[$(esc(los[c]))]
          end
          inited = true
        end
        for c in 1:n]...)
      end
    end
  end
end

# TODO count stationary, reset to 0 on jump

srand(999)
edges_xy = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
# edges_xy = ([1, 2, 3, 3, 4], [2, 3, 1, 4, 2])
edges_yz = deepcopy(edges_xy)
edges_xz = (copy(edges_xy[2]), copy(edges_xy[1]))
quicksort!(edges_xy)
quicksort!(edges_yz)
quicksort!(edges_xz)

function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  edges_xy_1, edges_xy_2 = edges_xy 
  edges_yz_1, edges_yz_2 = edges_yz 
  edges_xz_1, edges_xz_2 = edges_xz 
  xy_lo_1 = 1; xy_lo_2 = 1; xy_hi_1 = length(edges_xy_1); xy_hi_2 = length(edges_xy_2);
  yz_lo_1 = 1; yz_lo_2 = 1; yz_hi_1 = length(edges_yz_1); yz_hi_2 = length(edges_yz_2);
  xz_lo_1 = 1; xz_lo_2 = 1; xz_hi_1 = length(edges_xz_1); xz_hi_2 = length(edges_xz_2);
  xy_end = 1; yz_end = 1; xz_end = 1;
  ignore = 1;
  a = 0
  b = 0
  c = 0
  
  xy_lo_1 = 1; xy_hi_1 = length(edges_xy_1); 
  xz_lo_1 = 1; xz_hi_1 = length(edges_xz_1); 
  @time @intersect((edges_xy_1, edges_xz_1), (xy_lo_1, xz_lo_1), (xy_hi_1, xz_hi_1), (xy_lo_2, xz_lo_2), (xy_hi_2, xz_hi_2), begin
    a += 1
    # println("x=", edges_xy_1[xy_lo_1])
    yz_lo_1 = 1; yz_hi_1 = length(edges_yz_1)
    @intersect((edges_xy_2, edges_yz_1), (xy_lo_2, yz_lo_1), (xy_hi_2, yz_hi_1), (xy_end, yz_lo_2), (ignore, yz_hi_2), begin
      # println("  y=", edges_xy_2[xy_lo_2])
      b += 1
      @intersect((edges_yz_2, edges_xz_2), (yz_lo_2, xz_lo_2), (yz_hi_2, xz_hi_2), (yz_end, xz_end), (ignore, ignore), begin
        c += 1
        # println("    z=", edges_xz_2[xz_lo_2])
      end)
    end)
  end)
  (a, b, c)
end

@time f(edges_xy, edges_yz, edges_xz)

# @code_warntype f(edges_xy, edges_yz, edges_xz)
# @code_llvm gallop(edges_x[1], 3, 1, 3, <)

# readcsv(open("/home/jamie/soc-LiveJournal1.txt"))

end
