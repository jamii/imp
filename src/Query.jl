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

macro switch_on(ix_var, cases...)
  lines = [:(if $(esc(ix_var)) == $ix; $(esc(cases[ix])); end) for ix in 1:length(cases)]
  quote $(lines...); Void end
end

macro switch_off(ix_var, cases...)
  lines = [:(if $(esc(ix_var)) != $ix; $(esc(cases[ix])); end) for ix in 1:length(cases)]
  quote $(lines...); Void end
end

macro min_by_length(indexes, fingers)
  indexes = indexes.args
  fingers = fingers.args
  quote 
    min_ix = 1
    min_length = length($(esc(indexes[1])), $(esc(fingers[1])))
    $([quote 
      next_length = length($(esc(indexes[ix])), $(esc(fingers[ix])))
      if next_length < min_length
        min_ix = $ix
        min_length = next_length
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
      return_clause.typs[ix] = :(eltype($(return_clause.name).columns[$ix]))
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
      typ = :(Tuple{$(typs...)})
      index_init = quote
        local $(Symbol("index_$clause_ix"))::$typ = index($(esc(clause.name)), $order)
        local $(Symbol("finger_$(clause_ix)_1")) = finger($(Symbol("index_$clause_ix")))
      end
      push!(index_inits, index_init.args...)
    end
  end
  
  # initialize arrays for storing results
  results_inits = []
  for (var, typ) in zip(return_clause.vars, return_clause.typs)
    results_init = :(local $(Symbol("results_$var")) = Vector{$(esc(typ))}())
    push!(results_inits, results_init)
  end
  
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
  
  # store results 
  body = quote
    $([:(push!($(Symbol("results_$var")), $(esc(var))))
    for var in return_clause.vars]...)
    need_more_results = false
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
        need_more_results = true
        $body
      end
    end
    need_more_results = var_ix > return_after ? :need_more_results : true
    
    # find valid values for this variable
    clause = get(var_assigned_by, var, ())
    (clause_ixes, col_ixes) = index_sources[var]
    indexes = [Symbol("index_$clause_ix") for clause_ix in clause_ixes]
    fingers = [Symbol("finger_$(clause_ix)_$(col_ix)") for (clause_ix, col_ix) in zip(clause_ixes, col_ixes)]
    down_fingers = [Symbol("finger_$(clause_ix)_$(col_ix+1)") for (clause_ix, col_ix) in zip(clause_ixes, col_ixes)]
    projects = [:($down_finger = project($index, $finger, $(esc(var)))) for (index, finger, down_finger) in zip(indexes, fingers, down_fingers)]
    lengths = [:(length($index, $down_finger) > 0) for (index, down_finger) in zip(indexes, down_fingers)]
    if typeof(clause) == Assign
      body = quote
        let $(esc(var)) = $(esc(clause.expr))
          $(projects...)
          if $(reduce((a,b) -> :($a && $b), true, lengths))
            $body
          end
        end
      end
    elseif typeof(clause) == In
      body = quote
        let
          local iter = $(esc(clause.expr))
          local state = start(iter)
          local $(esc(var)) 
          while $need_more_results && !done(iter, state)
            $(projects...)
            ($(esc(var)), state) = next(iter, state)
            if $(reduce((a,b) -> :($a && $b), true, lengths))
              $body
            end
          end
        end
      end
    else 
      starts = [:($down_finger = start($index, $finger)) for (index, finger, down_finger) in zip(indexes, fingers, down_fingers)]
      heads = [:(head($index, $down_finger)) for (index, down_finger) in zip(indexes, down_fingers)]
      nexts = [:($down_finger = next($index, $finger, $down_finger)) for (index, finger, down_finger) in zip(indexes, fingers, down_fingers)]
      dones = [:(!done($index, $finger, $down_finger)) for (index, finger, down_finger) in zip(indexes, fingers, down_fingers)]
      body = quote 
        let 
          $([:(local $down_finger = Finger{$col_ix}(0,0)) for (down_finger, col_ix) in zip(down_fingers, col_ixes)]...)
          local ix = @min_by_length([$(indexes...)], [$(fingers...)])
          @switch_on ix $(starts...)
          local $(esc(var))
          local more = true
          while $need_more_results && more
            $(esc(var)) = @switch ix $(heads...)
            @switch_off ix $(projects...)
            if $(reduce((a,b) -> :($a && $b), lengths))
              $body
            end
            more = @switch ix $(dones...)
            if more
              @switch_on ix $(nexts...)
            end
          end
        end
      end
    end
    
  end
  
  # put everything together
  results_symbols = [Symbol("results_$var") for var in return_clause.vars]
  result = :(Relation(tuple($(results_symbols...)), $(return_clause.num_keys)))        
  quote 
    let
      $(index_inits...)
      $(results_inits...)
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
