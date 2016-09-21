module Query

using Data
using Match

macro switch(ix_var, cases...)
  body = :(error("Missing case in switch"))
  for ix in length(cases):-1:1
    body = :(($(esc(ix_var)) == $ix) ? $(esc(cases[ix])) : $body)
  end
  body
end

macro min_by_span(indexes, cols)
  indexes = indexes.args
  cols = cols.args
  quote 
    min_ix = 1
    min_span = span($(esc(indexes[1])), $(esc(cols[1])))
    $([quote 
      next_span = span($(esc(indexes[ix])), $(esc(cols[ix])))
      if next_span < min_span
        min_ix = $ix
        min_span = next_span
      end
    end for ix in 2:length(indexes)]...)
    min_ix
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
type Assign; var; expr; vars; end
type In; var; expr; vars; end
type Hint; var; end
type Return; name; vars; typs; num_keys; end  

function plan_query(query)
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
      line::Symbol => Hint(line)
      Expr(:call, [:in, var, expr], _) => In(var, expr, collect_vars(expr))
      Expr(:(=), [var, expr], _) => Assign(var, expr, collect_vars(expr))
      Expr(:macrocall, [head, expr], _) => @match head begin
        Symbol("@when") => When(expr, collect_vars(expr))
        _ => error("Don't know what to do with $head")
      end
      Expr(:return, [expr], _) => begin 
        (name, keys, vals) = Data.parse_relation(expr) 
        typed_vars = Any[keys..., vals...]
        Return(name, map(parse_var, typed_vars), map(parse_typ, typed_vars), length(keys)) 
      end
      _ => begin
        (name, keys, vals) = Data.parse_relation(line)
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
          insert!(clauses, 1, Assign(var, value, collect_vars(value)))
        end
      end
    end
    push!(clauses, clause)
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
  
  # collect vars mentioned in this query, in order of mention
  mentioned_vars = []
  for clause in clauses 
    if typeof(clause) in [Row, When, Assign, In]
      for var in clause.vars
        push!(mentioned_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In, Hint]
      push!(mentioned_vars, clause.var)
    end
  end
  
  # use mention order to decide execution order
  vars = unique((var for var in mentioned_vars if var in created_vars))
  
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
      return_clause.typs[ix] = :(eltype($(esc(return_clause.name)).columns[ix]))
    end
  end
  
  # collect clauses that assign a value to a var before intersect
  var_assigned_by = Dict()
  for clause in clauses
    if typeof(clause) in [Assign, In]
      @assert !haskey(var_assigned_by, clause.var) # only one assignment per var 
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
  
  index_sources = Dict(var => (Int64[], Int64[]) for var in vars)
  for (var, sources) in relation_sources
    for (clause_ix, var_ix) in sources
      col_ix = findfirst(sort_orders[clause_ix], var_ix) 
      push!(index_sources[var][1], clause_ix)
      push!(index_sources[var][2], col_ix)
    end
  end
  
  # --- codegen ---
  
  # for each Row clause, get the correct index
  index_inits = []
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      order = sort_orders[clause_ix]
      typs = [:(Vector{eltype($(esc(clause.name)).columns[$ix])}) for ix in order]
      typ = :(Index{Tuple{$(typs...)}})
      index_init = :(local $(Symbol("index_$clause_ix"))::$typ = index($(esc(clause.name)), $order))
      push!(index_inits, index_init)
    end
  end
  
  # for each var that is assigned an expr, make a wrapper function
  eval_funs = Dict()
  for (var, clause) in var_assigned_by
    args = intersect(vars, clause.vars)
    eval_fun = esc((quote
      @inline function $(Symbol("eval_$var"))($(args...))
        $(clause.expr)
      end
    end).args[2])
    eval_funs[var] = (args, eval_fun)
  end
  
  # for each When clause, make a wrapper function
  when_funs = Dict()
  for (ix, clause) in enumerate(clauses)
    if typeof(clause) == When
      args = intersect(vars, clause.vars)
      when_fun = esc((quote
        @inline function $(Symbol("when_$ix"))($(args...))
          $(clause.expr)
        end
      end).args[2])
      when_funs[ix] = (args, when_fun)
    end
  end
  
  # initialize arrays for storing results
  results_inits = []
  for (var, typ) in zip(return_clause.vars, return_clause.typs)
    results_init = :(local $(Symbol("results_$var")) = Vector{$typ}())
    push!(results_inits, results_init)
  end
  
  # combine all the init steps
  init = quote
    $(index_inits...)
    $((eval_funs[var][2] for var in vars if haskey(eval_funs, var))...)
    $((when_fun for (ix, (args, when_fun)) in when_funs)...)
    $(results_inits...)
  end
  
  # figure out at which point in the variable order each When clause can be run
  whens = [[] for _ in vars]
  for (clause_ix, (args, when_fun)) in when_funs
    var_ix = maximum(indexin(args, vars))
    when_call = :($(esc(Symbol("when_$clause_ix")))($(args...)))
    push!(whens[var_ix], when_call)
  end
  
  # figure out at which point in the variable order we have all the variables we need to return
  return_after = maximum(push!(indexin(return_clause.vars, vars), 0))
  
  # store results 
  body = quote
    $([:(push!($(Symbol("results_$var")), $var))
    for var in return_clause.vars]...)
    need_more_results = false
  end
  
  # build up the main loop from the inside out
  for var_ix in length(vars):-1:1
    var = vars[var_ix]
    
    # run any When clauses
    for when in whens[var_ix]
      body = :(if $when; $body; end)
    end
    
    # after return_after, only need to find one solution, not all solutions
    if var_ix == return_after
      body = quote
        need_more_results = true
        $body
      end
    end
    need_more_results = var_ix > return_after ? :need_more_results : true
    
    # find valid values for this variable
    clause = get(var_assigned_by, var, ())
    (args, eval_fun) = get(eval_funs, var, ([], ()))
    eval_call = :($(esc(Symbol("eval_$var")))($(args...)))
    (clause_ixes, col_ixes) = index_sources[var]
    indexes = [Symbol("index_$clause_ix") for clause_ix in clause_ixes]
    cols = [:(Val{$col_ix}) for col_ix in col_ixes]
    if typeof(clause) == Assign
      skips = [:(skip!($index, $col, $var)) for (ix, (index, col)) in enumerate(zip(indexes, cols))]
      body = quote
        let $var = $eval_call
          if $(reduce((a,b) -> :($a && $b), true, skips))
            $body
          end
        end
      end
    elseif typeof(clause) == In
      skips = [:(skip!($index, $col, $var)) for (ix, (index, col)) in enumerate(zip(indexes, cols))]
      body = quote
        let
          local iter = $eval_call
          local state = start(iter)
          local $var 
          while $need_more_results && !done(iter, state)
            ($var, state) = next(iter, state)
            if $(reduce((a,b) -> :($a && $b), true, skips))
              $body
            end
          end
        end
      end
    else 
      starts = [:(start!($index, $col)) for (index, col) in zip(indexes, cols)]
      dones = [:(span($index, $col) > 0) for (index, col) in zip(indexes, cols)]
      nexts = [:(next!($index, $col)) for (index, col) in zip(indexes, cols)]
      skips = [:((state_ix == $ix) || skip!($index, $col, $var)) for (ix, (index, col)) in enumerate(zip(indexes, cols))]
      body = quote 
        $(starts...)
        let state_ix = @min_by_span([$(indexes...)], [$(cols...)])
          while $need_more_results && (@switch state_ix $(dones...))
            let $var = @switch state_ix $(nexts...)
              if $(reduce((a,b) -> :($a && $b), skips))
                $body
              end
            end
          end
        end
      end
    end
    
  end
  
  results_symbols = [Symbol("results_$var") for var in return_clause.vars]
  result = :(Relation(tuple($(results_symbols...)), $(return_clause.num_keys)))        
  quote 
    let
      $init
      $body
      $(if return_clause.name != ()
        :(merge!($(esc(return_clause.name)), $result))
      else
        result
      end) 
    end
  end
end

macro query(query)
  plan_query(query)
end

export @query

end
