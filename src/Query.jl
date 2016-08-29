module Query

using Data

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

function get_variable_symbol(expr)
  if isa(expr, Expr) && expr.head == :(::)
    expr.args[1]
  else 
    expr
  end
end

function get_variable_type(expr)
  if isa(expr, Expr) && expr.head == :(::)
    expr.args[2]
  else 
    Any
  end
end

function plan_join(returned_typed_variables, aggregate, query)
  aggregate_zero, aggregate_add, aggregate_expr = aggregate
  aggregate_type = get_variable_type(aggregate_expr)
  
  returned_variables = map(get_variable_symbol, returned_typed_variables)
  returned_variable_types = Dict(zip(returned_variables, map(get_variable_type, returned_typed_variables)))
  
  grounded_variables = Set()
  variables = []
  relation_clauses = []
  expression_clauses = []
  assignment_clauses = Dict()
  loop_clauses = Dict()
  for (clause, line) in enumerate(query.args) 
    if isa(line, Symbol)
      push!(variables, line)
    elseif line.head == :call && line.args[1] == :in 
      variable = line.args[2]
      expr = line.args[3]
      assert(isa(variable, Symbol))
      assert(!haskey(loop_clauses, variable))
      push!(grounded_variables, variable)
      push!(variables, variable)
      push!(variables, collect_variables(expr)...)
      loop_clauses[variable] = expr
    elseif line.head == :call
      for (ix, arg) in enumerate(line.args)
        if ix > 1 && !isa(arg, Symbol)
          variable = gensym("variable")
          line.args[ix] = variable
          assignment_clauses[variable] = arg 
          insert!(variables, 1, variable) # only handles constants atm
          push!(grounded_variables, variable)
        elseif ix > 1 && isa(arg, Symbol)
          push!(variables, arg)
          push!(grounded_variables, arg)
        end
      end
      push!(relation_clauses, clause)
    elseif line.head == :(=)
      variable = line.args[1]
      expr = line.args[2]
      assert(isa(variable, Symbol))
      assert(!haskey(assignment_clauses, variable)) # only one assignment per var
      push!(grounded_variables, variable)
      push!(variables, variable)
      push!(variables, collect_variables(expr)...)
      assignment_clauses[variable] = expr
    elseif line.head == :macrocall && line.args[1] == symbol("@when")
      push!(expression_clauses, clause)
      push!(variables, collect_variables(line.args[2])...)
    else
      assert(line.head == :line)
    end
  end
  variables = unique([variable for variable in variables if variable in grounded_variables])
  @show variables
  
  sources = Dict([variable => [] for variable in variables])
  for clause in relation_clauses
    line = query.args[clause]
    for (column, variable) in enumerate(line.args[2:end])
      if variable in variables
        push!(sources[variable], (clause,column))
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
    index_init = :($(symbol("index_", clause)) = index($(symbol("relation_", clause)), [$(columns...)]))
    push!(index_inits, index_init)
  end
  
  column_inits = Vector(length(ixes))
  for ((clause, column), ix) in ixes
    if column == :buffer
      column_inits[ix] = :()
    else
      column_inits[ix] = :($(symbol("index_", clause))[$column])
    end
  end
  
  variable_inits = []
  for variable in variables
    clauses_and_columns = sources[variable]
    variable_ixes = [ixes[(clause, column)] for (clause, column) in clauses_and_columns]
    variable_columns = [:(columns[$ix]) for ix in variable_ixes]
    variable_init = quote
      $(symbol("columns_", variable)) = [$(variable_columns...)]
      $(symbol("ixes_", variable)) = [$(variable_ixes...)]
      $(if variable in returned_variables
          :($(symbol("results_", variable)) = Vector{$(returned_variable_types[variable])}())
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
    expr = query.args[clause].args[2]
    callable_at = maximum(indexin(collect_variables(expr), variables))
    push!(filters[callable_at], expr)
  end
  
  function make_return(returned_variables, tail) 
    quote
      aggregate = $aggregate_zero
      $tail 
      if aggregate != $aggregate_zero
        $([
        :(push!($(symbol("results_", variable)), $(esc(variable))))
        for variable in returned_variables]...)
        push!(results_aggregate, aggregate)
      end
    end
  end
  
  return_ix = 1 + maximum(push!(indexin(returned_variables, variables), 0))
  
  repeats = 1
  for buffer_ix in buffer_ixes
    repeats = :($repeats * (his[$buffer_ix] - los[$buffer_ix]))
  end
  body = :(aggregate = $(aggregate_add)(aggregate, $(esc(aggregate_expr)), $repeats))
  if return_ix == length(variables) + 1
    body = make_return(returned_variables, body)
  end
  for variable_ix in length(variables):-1:1
    variable = variables[variable_ix]
    variable_columns = symbol("columns_", variable)
    variable_ixes = symbol("ixes_", variable)
    for filter in filters[variable_ix]
      body = :(if $(esc(filter)); $body; end)
    end
    if haskey(assignment_clauses, variable)
      body = quote
        $(esc(variable)) = $(esc(assignment_clauses[variable]))
        if assign($variable_columns, los, ats, his, $variable_ixes, $(esc(variable)))
          $body
        end
      end
    elseif haskey(loop_clauses, variable)
      body = quote 
        for $(esc(variable)) in $(esc(loop_clauses[variable]))
          if assign($variable_columns, los, ats, his, $variable_ixes, $(esc(variable)))
            $body
          end
        end
      end
    else
      result_column = ixes[sources[variable][1]]
      body = quote
        start_intersect($variable_columns, los, ats, his, $variable_ixes)
        while next_intersect($variable_columns, los, ats, his, $variable_ixes)
          $(esc(variable)) = columns[$result_column][los[$(result_column+1)]]
          $body
        end
      end 
    end
    if return_ix == variable_ix 
      body = make_return(returned_variables, body)
    end
  end
  
  query_symbol = gensym("query")
          
  quote 
    # TODO pass through any external vars too to avoid closure boxing grossness
    function $query_symbol($([symbol("relation_", clause) for clause in relation_clauses]...))
      $setup
      $body
      Relation(tuple($([symbol("results_", variable) for variable in returned_variables]...), results_aggregate))
    end
    $query_symbol($([esc(query.args[clause].args[1]) for clause in relation_clauses]...))
  end
end

function plan_query(returned_typed_variables, aggregate, query)
  join = plan_join(returned_typed_variables, aggregate, query)

  project_aggregate = [aggregate[1], aggregate[2], :(prev_aggregate::$(get_variable_type(aggregate[3])))]  
  project_query = quote 
    intermediate($(map(get_variable_symbol, returned_typed_variables)...), prev_aggregate)
  end
  project = plan_join(returned_typed_variables, project_aggregate, project_query)
  
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end

@inline add_exp(a, b, n) = a + (b * n)
@inline mul_exp(a, b, n) = a * (b ^ n)
@inline min_exp(a, b, n) = min(a,b)

macro query(returned_typed_variables, query)
  aggregate = :(0, add_exp, 1::Int64)
  plan_query(returned_typed_variables.args, aggregate.args, query)
end

macro query(returned_typed_variables, aggregate, query)
  plan_query(returned_typed_variables.args, aggregate.args, query)
end

export @query, add_exp, mul_exp, min_exp

end
