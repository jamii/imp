module Imp

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

function start_intersect(cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  # println(cols)
  @inbounds begin
    for ix in ixes
      assert(los[ix] < his[ix])
      ats[ix] = los[ix]
    end
  end
end

function next_intersect(cols, los, ats, his, ixes)
  @inbounds begin
    fixed = 1
    n = length(ixes)
    if ats[ixes[n]] >= his[ixes[n]]
      return false
    else
      value = cols[n][ats[ixes[n]]]
    end
    while true 
      # println(value)
      for c in 1:n
        ix = ixes[c]
        if fixed == n
          for c2 in 1:n
            ix2 = ixes[c2]
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[c2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          return true
        else 
          ats[ix] = gallop(cols[c], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return false
        else 
          next_value = cols[c][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end

function assign(cols, los, ats, his, ixes, value)
  @inbounds begin
    n = length(ixes)
    for c in 1:n
      ix = ixes[c]
      los[ix+1] = gallop(cols[c], value, los[ix], his[ix], <)
      if los[ix+1] >= his[ix]
        return false
      end
      his[ix+1] = gallop(cols[c], value, los[ix+1], his[ix], <=)
    end
    return true
  end
end

function collect_variables(expr, variables)
  if isa(expr, Symbol)
    push!(variables, expr)
  elseif isa(expr, Expr) && expr.head != :quote
    for arg in expr.args
      collect_variables(arg, variables)
    end
  end
end

function collect_variables(expr)
  variables = []
  collect_variables(expr, variables)
  variables
end

function plan(returned_variables, typed_variables, aggregate, query)
  @show(returned_variables, typed_variables, aggregate, query)
  aggregate_zero, aggregate_add, aggregate_expr = aggregate
  if isa(aggregate_expr, Expr) && aggregate_expr.head == :(::)
    aggregate_type = aggregate_expr.args[2]
  else 
    aggregate_type = Any
  end
  
  variables = []
  variable_types = []
  for typed_variable in typed_variables
    if isa(typed_variable, Symbol)
      push!(variables, typed_variable)
      push!(variable_types, :Any)
    elseif isa(typed_variable, Expr) && typed_variable.head == :(::)
      push!(variables, typed_variable.args[1])
      push!(variable_types, typed_variable.args[2])
    else 
      throw("Variable must be a symbol (with optional type annotation)")
    end
  end 
  
  return_ix = 1 + maximum(push!(indexin(returned_variables, variables), 0))
  
  relations = [line.args[1] for line in query.args if line.head != :line]
  
  relation_clauses = []
  expression_clauses = []
  assignment_clauses = Dict()
  for (clause, line) in enumerate(query.args) 
    if line.head == :call && line.args[1] in [:<, :>, :(==), :<=, :>=]
      push!(expression_clauses, clause)
    elseif line.head == :call
      push!(relation_clauses, clause)
    elseif line.head == :(=)
      variable = line.args[1]
      assert(isa(variable, Symbol)) # single assignment only, no unpacking yet
      assert(get(assignment_clauses, variable, nothing) == nothing) # only one assignment per var
      assignment_clauses[variable] = line.args[2]
    else
      assert(line.head == :line)
    end
  end
  
  sources = Dict()
  for clause in relation_clauses
    line = query.args[clause]
    for (column, variable) in enumerate(line.args[2:end])
      if variable in variables
        push!(get!(()->[], sources, variable), (clause,column))
      end
    end
  end
  
  sort_orders = Dict()
  for variable in variables 
    for (clause, column) in sources[variable]
      push!(get!(()->[], sort_orders, clause), column)
    end
  end
  
  ixes = Dict()
  next_ix = 1
  for (clause, columns) in sort_orders
    for column in columns
      ixes[(clause, column)] = next_ix
      next_ix += 1
    end
    ixes[(clause, :buffer)] = next_ix
    next_ix += 1
  end
  
  column_inits = Vector(length(ixes))
  for ((clause, column), ix) in ixes
    if column == :buffer
      column_inits[ix] = :()
    else
      clause_name = query.args[clause].args[1]
      column_inits[ix] = :(copy($(esc(clause_name))[$column]))
    end
  end
  
  sorts = []
  for (clause, columns) in sort_orders
    sort_ixes = [ixes[(clause, column)] for column in columns]
    sort_args = [:(columns[$ix]) for ix in sort_ixes]
    sort = :(quicksort!(tuple($(sort_args...))))
    push!(sorts, sort)
  end
  
  variable_inits = []
  for (variable, variable_type) in zip(variables, variable_types)
    clauses_and_columns = sources[variable]
    variable_ixes = [ixes[(clause, column)] for (clause, column) in clauses_and_columns]
    variable_columns = [:(columns[$ix]) for ix in variable_ixes]
    variable_init = quote
      $(symbol("columns_", variable)) = [$(variable_columns...)]
      $(symbol("ixes_", variable)) = [$(variable_ixes...)]
      $(if variable in returned_variables
          :($(symbol("results_", variable)) = Vector{$(variable_type)}())
        end)
    end
    push!(variable_inits, variable_init)
  end
  
  setup = quote
    columns = tuple($(column_inits...))
    $(sorts...)
    los = Int64[1 for i in 1:$(length(ixes))]
    ats = Int64[1 for i in 1:$(length(ixes))]
    his = Int64[length(columns[i])+1 for i in 1:$(length(ixes))]
    $(variable_inits...)
    results_aggregate = Vector{$aggregate_type}()
  end
  
  filters = [[] for _ in variables]
  for clause in expression_clauses
    line = query.args[clause]
    callable_at = maximum(indexin(collect_variables(line), variables))
    push!(filters[callable_at], line)
  end
  
  function make_filter(filters, tail)
    if length(filters) == 0
      tail
    else 
      quote 
        if $(filters[1])
          $(make_filter(filters[2:end], tail))
        end
      end
    end
  end
  
  function make_return(variable_ix, tail) 
    if variable_ix == return_ix
      quote
        aggregate = $aggregate_zero
        $tail 
        if aggregate != $aggregate_zero
          $([
          :(push!($(symbol("results_", variable)), $variable))
          for variable in returned_variables]...)
          push!(results_aggregate, aggregate)
        end
      end
    else
      tail
    end
  end
  
  function make_intersect(variable_ix, tail)
    variable = variables[variable_ix]
    variable_columns = symbol("columns_", variable)
    variable_ixes = symbol("ixes_", variable)
    result_column = ixes[sources[variable][1]]
    if haskey(assignment_clauses, variable)
      quote
        $variable = $(assignment_clauses[variable])
        if assign($variable_columns, los, ats, his, $variable_ixes, $variable)
          $tail
        end
      end
    else
      quote
        start_intersect($variable_columns, los, ats, his, $variable_ixes)
        while next_intersect($variable_columns, los, ats, his, $variable_ixes)
          $variable = columns[$result_column][los[$(result_column+1)]]
          $tail
        end
      end
    end
  end
  
  function body(variable_ix)
    if variable_ix < length(variables)
      tail = body(variable_ix + 1)
    else 
      tail = make_return(variable_ix + 1, :(aggregate = $(aggregate_add)(aggregate, $aggregate_expr)))
    end
    make_return(variable_ix, make_intersect(variable_ix, make_filter(filters[variable_ix], tail)))
  end
          
  quote 
    $setup
    $(body(1))
    tuple($([symbol("results_", variable) for variable in returned_variables]...), results_aggregate)
  end
end

macro join(returned_variables, typed_variables, query)
  plan(returned_variables.args, typed_variables.args, :(0, +, 1::Int64).args, query)
end

macro join(returned_variables, typed_variables, aggregate, query)
  plan(returned_variables.args, typed_variables.args, aggregate.args, query)
end

# TODO relies on ordering in returned/typed being the same
# TODO only accepts typed variables
macro query(returned_variables, typed_variables, aggregate, query)
  result_aggregate = [aggregate.args[1], aggregate.args[2], :(aggregate::$(aggregate.args[3].args[2]))]
  variables = [v.args[1] for v in typed_variables.args]
  last_returned_ix = maximum(push!(indexin(returned_variables.args, variables), 0))
  result_typed_variables = copy(typed_variables.args[1:last_returned_ix])
  push!(result_typed_variables, :(aggregate::$(aggregate.args[3].args[2])))
  result_query_variables = [v.args[1] for v in result_typed_variables]
  quote 
    let $(esc(:result)) = $(plan(returned_variables.args, typed_variables.args, aggregate.args, query))
      $(plan(returned_variables.args, result_typed_variables, result_aggregate, 
      quote
        result($(result_query_variables...))
      end))
    end
  end
end

srand(999)
# edge = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
edge = ([1, 2, 3, 3, 4], [2, 3, 1, 4, 2])

function f(edge) 
  @join([a,b,c], [a::Int64,b::Int64,c::Int64], 
  begin
    edge(a,b)
    a < b
    edge(b,c)
    b < c
    edge(c,a)
  end)
end

macroexpand(quote 
@join([a,b,c], [a::Int64,b::Int64,c::Int64], 
begin
  edge(a,b)
  a < b
  edge(b,c)
  b < c
  edge(c,a)
end)
end)

# @code_warntype f(edge)
f(edge)
f(edge)

function read_columns(filename, types)
  rows, _ = readdlm(open(filename), '\t', header=true, quotes=false, comments=false)
  n = length(types)
  columns = tuple([Vector{types[c]}() for c in 1:n]...)
  for r in 1:size(rows)[1]
    for c in 1:n
      if types[c] == String
        push!(columns[c], string(rows[r, c]))
      else
        push!(columns[c], rows[r, c])
      end
    end
  end
  columns
end

album = read_columns("data/Album.csv", [Int64, String, Int64])
artist = read_columns("data/Artist.csv", [Int64, String])
track = read_columns("data/Track.csv", [Int64, String, Int64, Int64, Int64, String, Int64, Int64, Float64])
playlist_track = read_columns("data/PlaylistTrack.csv", [Int64, Int64])
playlist = read_columns("data/Playlist.csv", [Int64, String])

macroexpand(quote
@join([an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end)
end)

function who_is_metal(album, artist, track, playlist_track, playlist)
  metal = @join([an],
  [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end)
  @join([an],
  [an::String], 
  begin
    metal(an)
  end)
end

function who_is_metal2(album, artist, track, playlist_track, playlist)
  i1 = @join([p],
  [pn::String, p::Int64],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
  end)
  i2 = @join([t],
  [p::Int64, t::Int64],
  begin 
    i1(p)
    playlist_track(p, t)
  end)
  i3 = @join([al],
  [t::Int64, al::Int64],
  begin 
    i2(t)
    track(t, _, al)
  end)
  i4 = @join([a],
  [al::Int64, a::Int64],
  begin 
    i3(al)
    album(al, _, a)
  end)
  i5 = @join([an],
  [a::Int64, an::String],
  begin 
    i4(a)
    artist(a, an)
  end)
  @join([an],
  [an::String],
  begin
    i5(an)
  end)
end

who_is_metal(album, artist, track, playlist_track, playlist)

assert(who_is_metal(album, artist, track, playlist_track, playlist) == who_is_metal2(album, artist, track, playlist_track, playlist))

# @code_warntype who_is_metal(album, artist, track, playlist_track, playlist)

# @time [who_is_metal(album, artist, track, playlist_track, playlist) for n in 1:10000]
# @time [who_is_metal2(album, artist, track, playlist_track, playlist) for n in 1:10000]

function how_metal(album, artist, track, playlist_track, playlist)
  metal = @join([],
  [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end)
end

@time how_metal(album, artist, track, playlist_track, playlist)

function cost_of_playlist(album, artist, track, playlist_track, playlist)
  @join([pn],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,+,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
end

@time cost_of_playlist(album, artist, track, playlist_track, playlist)

function revenue_per_track(album, artist, track, playlist_track, playlist)
  result = @join([p, pn, t],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,+,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
  @join([t],
  [t::Int64, p::Int64, pn::String, price::Float64],
  (0.0,+,price::Float64),
  begin
    result(p, pn, t, price)
  end)
end

function revenue_per_track2(album, artist, track, playlist_track, playlist)
  @query([t],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,+,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
end

@time revenue_per_track(album, artist, track, playlist_track, playlist)
@time revenue_per_track2(album, artist, track, playlist_track, playlist)

end
