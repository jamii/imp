module Query

using Data
using Match

# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::Vector{T}, value::T, lo::Int, hi::Int, threshold) 
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

function project(columns, los, his, next_los, next_his, var, body)
  for (column, lo, hi, next_lo, next_hi) in zip(columns, los, his, next_los, next_his)
    body = quote
      local $next_lo
      local $next_hi
      $next_lo, c = gallop($column, $var, $lo, $hi, 0)
      if (c == 0)
        $next_hi, _ = gallop($column, $var, $next_lo+1, $hi, 1)
        $body
      end
    end
  end
  body
end

function intersect(columns, los, his, next_los, next_his, var, body)
  n = length(columns)
  columns_rot = [columns[1+mod(ix-2,n)] for ix in 1:n]
  next_los_rot = [next_los[1+mod(ix-2,n)] for ix in 1:n]
  total = gensym("total")
  quote
    begin
      $([:(local $next_hi, $next_lo = $lo) for (next_hi, next_lo, lo) in zip(next_his, next_los, los)]...)
      $total = 1
      while true
        if $total == $n
          $([:(($next_hi, _) = gallop($column, $column[$next_lo], $next_lo+1, $hi, 1)) for (next_hi, column, next_lo, hi) in zip(next_his, columns, next_los, his)]...)
          $var = $(columns[1])[$(next_los[1])]
          $body
          $([:($next_lo = $next_hi) for (next_lo, next_hi) in zip(next_los, next_his)]...)
          $([:(if $next_lo >= $hi; break; end) for (next_lo, hi) in zip(next_los, his)]...)
          $total = 1
        end
        $([quote
        if $total < $n
          $next_lo, c = gallop($column, $column_rot[$next_lo_rot], $next_lo, $hi, 0)
          $total = (c == 0) ? $total + 1 : 1
          if $next_lo >= $hi; break; end
        end
      end for (next_lo, column, column_rot, next_lo_rot, hi) in zip(next_los, columns, columns_rot, next_los_rot, his)]...)
      end
    end
  end
end
  
function collect_vars(expr, vars)
  if isa(expr, Symbol)
    push!(vars, expr)
  elseif isa(expr, Expr) && expr.head != :quote
    for arg in expr.args
      collect_vars(arg, vars)
    end
  end
end

function collect_vars(expr)
  vars = []
  collect_vars(expr, vars)
  vars
end

function parse_var(expr)
  @match expr begin
    Expr(:(::), [var, typ], _) => var
    _ => expr
  end
end

function parse_typ(expr)
  @match expr begin
    Expr(:(::), [var, typ], _) => typ
    _ => :Any
  end
end
  
type Row; name; vars; num_keys; end
type When; expr; vars; end
type Assign; var; expr; vars; escape end
type In; var; expr; vars; end
type Hint; vars; end
type Return; name; vars; typs; num_keys; end  
type SubQuery; query; var; clauses; vars; created_vars; input_names; return_clause; end

function parse_query(query)
  # unwrap block
  lines = @match query begin
    Expr(:block, lines, _) => lines
    line => [line]
  end
  
  # parse
  clauses = []
  for line in lines
    clause = @match line begin
      Expr(:line, _, _) => :line
      Expr(:call, [:in, var, expr], _) => In(var, expr, collect_vars(expr))
      Expr(:(=), [var, expr], _) => Assign(var, expr, collect_vars(expr), true)
      Expr(:macrocall, [head, args...], _) => @match head begin
        Symbol("@when") => When(args[1], collect_vars(args[1]))
        Symbol("@hint") => Hint(args)
        Symbol("@query") => SubQuery(line, gensym("query"), parse_query(args[1])...)
        _ => error("Don't know what to do with $head")
      end
      Expr(:return, [expr], _) => begin 
        (name, keys, vals) = parse_relation(expr) 
        typed_vars = Any[keys..., vals...]
        Return(name, map(parse_var, typed_vars), map(parse_typ, typed_vars), length(keys)) 
      end
      _ => begin
        (name, keys, vals) = parse_relation(line)
        Row(name, Any[keys..., vals...], length(keys))
      end
    end
    if clause != :line
      push!(clauses, clause)
    end
  end
  
  # check all assignments are to single vars
  for clause in clauses
    if typeof(clause) in [In, Assign]
      @assert isa(clause.var, Symbol)
    end
  end
  
  # rewrite expressions nested in Row
  old_clauses = clauses
  clauses = []
  for clause in old_clauses
    if typeof(clause) in [Row, Return]
      for (ix, expr) in enumerate(clause.vars)
        if !isa(expr, Symbol)
          var = gensym("constant")
          clause.vars[ix] = var
          value = @match expr begin
            Expr(:$, [value], _) => value
            value => value 
          end
          push!(clauses, Assign(var, value, collect_vars(value), true))
        end
      end
    end
    push!(clauses, clause)
  end
  
  # collect relation names
  input_names = Set{Symbol}()
  for clause in clauses
    if typeof(clause) == Row 
      push!(input_names, clause.name)
    elseif typeof(clause) == SubQuery
      for name in clause.input_names
        push!(input_names, name)
      end
    elseif typeof(clause) == Return
      if clause.name != ()
        push!(input_names, clause.name)
      end
    end
  end
  
  # collect vars created in this query
  created_vars = Set()
  for clause in clauses
    if typeof(clause) in [Row]
      for var in clause.vars
        push!(created_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In]
      push!(created_vars, clause.var)
    end
  end
  delete!(created_vars, :_) # _ is a wildcard, not a real var
  
  # collect vars created in subqueries
  subcreated_vars = Set()
  for clause in clauses
    if typeof(clause) == SubQuery
      push!(created_vars, clause.var)
      for var in clause.return_clause.vars
        push!(subcreated_vars, var)
      end
    end
  end
  
  # collect vars mentioned in this query, in order of mention
  mentioned_vars = []
  for clause in clauses 
    if typeof(clause) in [Row, When, Assign, In, Hint]
      for var in clause.vars
        push!(mentioned_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In]
      push!(mentioned_vars, clause.var)
    end
    if typeof(clause) == SubQuery
      push!(mentioned_vars, clause.var)
      for var in clause.vars
        push!(mentioned_vars, var)
      end
    end
  end
  
  # use mention order to decide execution order
  vars = unique((var for var in mentioned_vars if (var in created_vars) || (var in subcreated_vars)))
  
  # add a return if needed
  returns = [clause for clause in clauses if typeof(clause) == Return]
  if length(returns) == 0
    return_clause = Return((), vars, [:Any for _ in vars], length(vars))
  elseif length(returns) == 1
    return_clause = returns[1]
  else
    error("Too many returns: $returns")
  end
  
  # use types of return relation if available
  if return_clause.name != ()
    for (ix, var) in enumerate(return_clause.vars)
      return_clause.typs[ix] = :(eltype($(return_clause.name)[$ix]))
    end
  end
  
  (clauses, vars, created_vars, input_names, return_clause)
end

function plan_query(clauses, vars, created_vars, input_names, return_clause, outside_vars)
  # rewrite SubQuery in terms of Assign
  for clause_ix in length(clauses):-1:1
    clause = clauses[clause_ix]
    if typeof(clause) == SubQuery
      deleteat!(clauses, clause_ix)
      code = plan_query(clause.clauses, clause.vars, clause.created_vars, clause.input_names, clause.return_clause, created_vars)
      for (var_ix, return_var) in enumerate(clause.return_clause.vars)
        if !(return_var in created_vars)
          insert!(clauses, clause_ix, Assign(return_var, :($(clause.var)[$var_ix]), [clause.var], true))
        end
      end
      insert!(clauses, clause_ix, Assign(clause.var, code, clause.vars, false))
    end
  end
  
  # collect clauses that assign a value to a var before intersect
  var_assigned_by = Dict()
  for clause in clauses
    if typeof(clause) in [Assign, In]
      @assert !haskey(var_assigned_by, clause.var) # only one assignment per var 
      @assert !(clause.var in outside_vars) # can't reassign an existing var
      var_assigned_by[clause.var] = clause
    end
  end
  
  # for each var, collect list of relation/column pairs that need to be intersected
  relation_sources = Dict(var => Tuple{Int64, Int64}[] for var in vars)
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      for (var_ix, var) in enumerate(clause.vars)
        if var != :_
          push!(relation_sources[var], (clause_ix, var_ix))
        end
      end
    end
  end
  
  # for each Row clause, figure out what order to sort the index in
  sort_orders = Dict(clause_ix => Int64[] for clause_ix in 1:length(clauses))
  for var in vars 
    for (clause_ix, var_ix) in relation_sources[var]
      push!(sort_orders[clause_ix], var_ix)
    end
  end
  
  # --- codegen ---
  
  # for each Row clause, get the correct index and create initial ranges
  index_inits = []
  index_checks = []
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      sort_order = sort_orders[clause_ix]
      index_init = :(local $(Symbol("index_$clause_ix")) = index($(esc(clause.name)), $sort_order))
      columns = [:(local $(Symbol("index_$(clause_ix)_$(var_ix)")) = $(Symbol("index_$clause_ix"))[$var_ix]) for var_ix in sort_order]
      lo_init = :(local $(Symbol("lo_$(clause_ix)_0")) = 1)
      hi_init = :(local $(Symbol("hi_$(clause_ix)_0")) = 1 + length($(Symbol("index_$(clause_ix)_$(first(sort_order))"))))
      push!(index_inits, index_init, columns..., lo_init, hi_init)
      index_check = :($(Symbol("lo_$(clause_ix)_0")) < $(Symbol("hi_$(clause_ix)_0")))
      push!(index_checks, index_check)
    end
  end  
  
  # initialize arrays for storing results
  results_inits = []
  for (ix, typ) in enumerate(return_clause.typs)
    results_init = :(local $(Symbol("results_$ix")) = Vector{$(esc(typ))}())
    push!(results_inits, results_init)
  end
  
  # declare vars
  var_inits = [:(local $(esc(var))) for var in vars if !(var in outside_vars)]
  
  # figure out at which point in the variable order each When clause can be run
  whens = [[] for _ in vars]
  for clause in clauses
    if typeof(clause) == When
      var_ix = maximum(indexin(clause.vars, vars))
      push!(whens[var_ix], clause.expr)
    end
  end
  
  # figure out at which point in the variable order we have all the variables we need to return
  return_after = maximum(push!(indexin(return_clause.vars, vars), 0))
  
  # label for early return
  next_result = gensym("next_result")
  
  # store results 
  body = quote
    $([:(push!($(Symbol("results_$ix")), $(esc(var))))
    for (ix, var) in enumerate(return_clause.vars)]...)
    @goto $next_result
  end
  
  # build up the main loop from the inside out
  for var_ix in length(vars):-1:1
    var = vars[var_ix]
    
    # run any When clauses
    for when in whens[var_ix]
      body = :(if $(esc(when)); $body; end)
    end
    
    # after return_after, only need to find one solution, not all solutions
    if var_ix == return_after
      body = quote
        $body
        @label $next_result
      end
    end
    
    # run the intersection for this variable
    clause = get(var_assigned_by, var, ())
    columns = [Symbol("index_$(clause_ix)_$(var_ix)") for (clause_ix, var_ix) in relation_sources[var]]
    range_ixes = [(clause_ix, findfirst(sort_orders[clause_ix], var_ix)) for (clause_ix, var_ix) in relation_sources[var]]
    los = [Symbol("lo_$(clause_ix)_$(range_ix-1)") for (clause_ix, range_ix) in range_ixes]
    next_los = [Symbol("lo_$(clause_ix)_$(range_ix)") for (clause_ix, range_ix) in range_ixes]
    his = [Symbol("hi_$(clause_ix)_$(range_ix-1)") for (clause_ix, range_ix) in range_ixes]
    next_his = [Symbol("hi_$(clause_ix)_$(range_ix)") for (clause_ix, range_ix) in range_ixes]
    if var in outside_vars
      body = project(columns, los, his, next_los, next_his, esc(var), body)
    elseif typeof(clause) == Assign
      expr = clause.escape ? esc(clause.expr) : clause.expr
      body = :(let $(esc(var)) = $expr
        $(project(columns, los, his, next_los, next_his, esc(var), body))
      end)
    elseif typeof(clause) == In
      body = :(for $(esc(var)) in $(esc(clause.expr))
        $(project(columns, los, his, next_los, next_his, esc(var), body))
      end)
    else
      body = intersect(columns, los, his, next_los, next_his, esc(var), body)
    end
  end
  
  # put everything together
  results_symbols = [Symbol("results_$ix") for (ix, var) in enumerate(return_clause.vars)]
  result = :(Relation(tuple($(results_symbols...)), $(return_clause.num_keys))) 
  quote 
    let
      $(index_inits...)
      $(results_inits...)
      if $(reduce((a,b) -> :($a && $b), true, index_checks))
        let 
          $(var_inits...) # declare vars local in here so they can't shadow relation names
          $body 
        end
      end
      $result
    end
  end
end

macro query(query)
  plan_query(parse_query(query)..., Set())
end

export @query, gallop

end
