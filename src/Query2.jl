module Query

using Data
using Match

# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::AbstractArray{T}, value::T, lo::Int, hi::Int, threshold) 
  c = -1
  @inbounds if (lo < hi) && (c2 = cmp(column[lo], value); c *= c2; c2 < threshold)
    step = 1
    while (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && (c2 = cmp(column[lo + step], value); c *= c2; c2 < threshold)
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  (lo, c) 
end 

# Vector{Column}
# Vector{Range}
# buffer in intersect for old ranges

#   Intersect(columns, rest)
#   old_ranges = (...)
# for range in intersect(columns)
#   ranges[i] = range
#   eval(rest, ranges)
#   ranges = old_ranges

#   Intersect(column_ixes, range_ixes, rest)

struct Intersect{T}
  column_ixes::Vector{Int64}
  range_ixes::Vector{Int64}
  tail::T
end

struct Return
  result_ix::Int64
  column_ixes::Vector{Int64}
  range_ixes::Vector{Int64}
end

function solve(query::Return, columns, ranges, results)
  result = results[query.result_ix]
  for (i, (column_ix, range_ix)) in enumerate(zip(query.column_ixes, query.range_ixes))
    push!(result[i], columns[column_ix][ranges[range_ix].start])
  end
end

function solve(query::Intersect, columns, ranges, results)
  los = [ranges[range_ix].start for range_ix in query.range_ixes]
  his = [ranges[range_ix].stop for range_ix in query.range_ixes]
  next_los = copy(los)
  next_his = copy(his)
  n = length(query.range_ixes)
  total = 1
  while true
    @show total los his next_los next_his
    if total == n
      for i in 1:n
        column = columns[query.column_ixes[i]]
        (next_his[i], _) = gallop(column, column[next_los[i]], next_los[i]+1, his[i], 1)
        ranges[query.range_ixes[i]] = next_los[i]:next_his[i]
        next_los[i] = next_his[i]
      end
      solve(query.tail, columns, ranges, results)
      total = 1
    end
    for i in 1:n
      if total < n
        prev_i = 1+mod(i-2,n)
        column = columns[query.column_ixes[i]]
        prev_column = columns[query.column_ixes[prev_i]]
        (next_los[i], c) = gallop(column, prev_column[next_los[prev_i]], next_los[i], his[i], 0)
        total = (c == 0) ? total + 1 : 1
        if next_los[i] >= his[i]
          more = false;
        end
      end
    end
  end
  for (i, range_ix) in enumerate(query.range_ixes)
    ranges[range_ix] = los[i]:his[i]
  end
end

columns = ([1,2,3],["a", "b", "c"],["a","b","b"],["apple","banana","box"])

ranges = [1:3, 1:3]

results = ((Int64[], String[]),)
query = Return(1, [1,4],[1,2])
solve(query, columns, ranges, results)
@show results

results = ((Int64[], String[]),)
query = Intersect([2,3], [1,2], Return(1, [1,4],[1,2]))
solve(query, columns, ranges, results)
@show results

results = ((Int64[], String[]),)
query = Intersect([2,3], [1,2], Intersect([4],[2], Return(1, [1,4],[1,2])))
solve(query, columns, ranges, results)
@show results

end
