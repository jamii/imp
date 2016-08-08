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

type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
end

function Relation{T}(columns::T)
  Relation(columns, Dict{Vector{Int64},T}())
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  get!(relation.indexes, order) do
    index::T = tuple([(ix in order) ? copy(column) : Vector{eltype(column)}() for (ix, column) in enumerate(relation.columns)]...)
    quicksort!(tuple([index[ix] for ix in order]...))
    index
  end
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

function analyse(returned_variables, typed_variables, aggregate)
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
  
  (aggregate_type, variables, variable_types, return_ix)
end

function plan_join(returned_variables, aggregate, aggregate_type, variables, variable_types, return_ix, query)
  aggregate_zero, aggregate_add, aggregate_expr = aggregate
  
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
  
  relation_names = Dict()
  for clause in relation_clauses
    relation_name = query.args[clause].args[1]
    relation_names[clause] = relation_name
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
  buffer_ixes = []
  next_ix = 1
  for (clause, columns) in sort_orders
    for column in columns
      ixes[(clause, column)] = next_ix
      next_ix += 1
    end
    ixes[(clause, :buffer)] = next_ix
    push!(buffer_ixes, next_ix)
    next_ix += 1
  end
  
  index_inits = []
  for (clause, columns) in sort_orders
    relation_name = relation_names[clause]
    index_init = :($(symbol("index_", relation_name)) = index($relation_name, [$(columns...)]))
    push!(index_inits, index_init)
  end
  
  column_inits = Vector(length(ixes))
  for ((clause, column), ix) in ixes
    if column == :buffer
      column_inits[ix] = :()
    else
      relation_name = relation_names[clause]
      column_inits[ix] = :($(symbol("index_", relation_name))[$column])
    end
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
    $(index_inits...)
    columns = tuple($(column_inits...))
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
  
  function make_return(returned_variables, tail) 
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
  end
  
  repeats = 1
  for buffer_ix in buffer_ixes
    repeats = :($repeats * (his[$buffer_ix] - los[$buffer_ix]))
  end
  body = :(aggregate = $(aggregate_add)(aggregate, $aggregate_expr, $repeats))
  if return_ix == length(variables) + 1
    body = make_return(returned_variables, body)
  end
  for variable_ix in length(variables):-1:1
    variable = variables[variable_ix]
    variable_columns = symbol("columns_", variable)
    variable_ixes = symbol("ixes_", variable)
    result_column = ixes[sources[variable][1]]
    for filter in filters[variable_ix]
      body = :(if $filter; $body; end)
    end
    if haskey(assignment_clauses, variable)
      body = quote
        $variable = $(assignment_clauses[variable])
        if assign($variable_columns, los, ats, his, $variable_ixes, $variable)
          $body
        end
      end
    else
      body = quote
        start_intersect($variable_columns, los, ats, his, $variable_ixes)
        while next_intersect($variable_columns, los, ats, his, $variable_ixes)
          $variable = columns[$result_column][los[$(result_column+1)]]
          $body
        end
      end 
    end
    if return_ix == variable_ix 
      body = make_return(returned_variables, body)
    end
  end
  
  escaped_names = Set(values(relation_names))
          
  quote 
    # TODO pass through any external vars too to avoid closure boxing grossness
    function query($(escaped_names...))
      $setup
      $body
      Relation(tuple($([symbol("results_", variable) for variable in returned_variables]...), results_aggregate))
    end
    query($([esc(name) for name in escaped_names]...))
  end
end


function plan_query(returned_variables, typed_variables, aggregate, query)
  aggregate_type, variables, variable_types, return_ix = analyse(returned_variables, typed_variables, aggregate)
  join = plan_join(returned_variables, aggregate, aggregate_type, variables, variable_types, return_ix, query)
  project_variables = Any[variable for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  project_variable_types = Any[variable_type for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  push!(project_variables, :prev_aggregate)
  push!(project_variable_types, aggregate_type)
  project_aggregate = [aggregate[1], aggregate[2], :prev_aggregate]
  project_query = quote 
    intermediate($(project_variables...))
  end
  project_return_ix = length(returned_variables) + 1
  project = plan_join(returned_variables, project_aggregate, aggregate_type, project_variables, project_variable_types, project_return_ix, project_query)
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end

@inline add_exp(a, b, n) = a + (b * n)
@inline mul_exp(a, b, n) = a * (b ^ n)

macro join(returned_variables, typed_variables, query)
  :(@join($returned_variables, $typed_variables, (0, add_exp, 1::Int64), $query))
end

macro join(returned_variables, typed_variables, aggregate, query)
  aggregate_type, variables, variable_types, return_ix = analyse(returned_variables.args, typed_variables.args, aggregate.args)
  plan_join(returned_variables.args, aggregate.args, aggregate_type, variables, variable_types, return_ix, query)
end

macro query(returned_variables, typed_variables, query)
  :(@query($returned_variables, $typed_variables, (0, add_exp, 1::Int64), $query))
end

macro query(returned_variables, typed_variables, aggregate, query)
  plan_query(returned_variables.args, typed_variables.args, aggregate.args, query)
end

function read_columns(filename, types; comments=false)
  rows, _ = readdlm(open(filename), '\t', header=true, quotes=false, comments=comments)
  n = length(types)
  columns = tuple([Vector{types[c]}() for c in 1:n]...)
  for r in 1:size(rows)[1]
    for c in 1:n
      if types[c] == String
        push!(columns[c], string(rows[r, c]))
      elseif isa(rows[r, c], SubString)
        @show r c rows[r, c]
      else
        push!(columns[c], rows[r, c])
      end
    end
  end
  Relation(columns)
end

import Atom
function Atom.render(editor::Atom.Editor, relation::Relation)
  Atom.render(editor, relation.columns)
end

srand(999)
# edge = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
edge = Relation(([1, 2, 3, 3, 4], [2, 3, 1, 4, 2]))
# edge = read_columns("/home/jamie/soc-LiveJournal1.txt", [Int32, Int32], comments=true)

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
who_is_metal2(album, artist, track, playlist_track, playlist)

# assert(who_is_metal(album, artist, track, playlist_track, playlist) == who_is_metal2(album, artist, track, playlist_track, playlist))

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
  (0.0,add_exp,price::Float64),
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
  (0.0,add_exp,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
  @join([t],
  [t::Int64, p::Int64, pn::String, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    result(p, pn, t, price)
  end)
end

function revenue_per_track2(album, artist, track, playlist_track, playlist)
  @query([t],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
end

@time revenue_per_track(album, artist, track, playlist_track, playlist)
@time revenue_per_track2(album, artist, track, playlist_track, playlist)

assert(revenue_per_track(album, artist, track, playlist_track, playlist).columns == revenue_per_track2(album, artist, track, playlist_track, playlist).columns)

@query([pn, an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end).columns

end
