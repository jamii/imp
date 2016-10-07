module Query

using Data
using Match
using Base.Cartesian

@generated function project_at{N}(intersection, ranges::NTuple{N}, val, min)
  :(@ntuple $N ix -> 
  if ix != min
    project(intersection[ix], ranges[ix], val)
  else 
    ranges[ix].start:gallop(intersection[ix], val, ranges[ix].start+1, ranges[ix].stop, 1)
  end)
end

@generated function feasible_at{N}(ranges::NTuple{N})
  :(@nall $N ix -> ranges[ix].start < ranges[ix].stop)
end

@generated function next_at{N}(state::NTuple{N}, ranges::NTuple{N})
  :(@ntuple $N ix -> ranges[ix].stop:state[ix].stop)
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
type Hint; vars; end
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
      Expr(:call, [:in, var, expr], _) => In(var, expr, collect_vars(expr))
      Expr(:(=), [var, expr], _) => Assign(var, expr, collect_vars(expr))
      Expr(:macrocall, [head, args...], _) => @match head begin
        Symbol("@when") => When(args[1], collect_vars(args[1]))
        Symbol("@hint") => Hint(args)
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
    if typeof(clause) in [Row, When, Assign, In, Hint]
      for var in clause.vars
        push!(mentioned_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In]
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
      return_clause.typs[ix] = :(eltype($(return_clause.name)[$ix]))
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
  
  # --- codegen ---
  
  # for each Row clause, get the correct index and create initial ranges
  index_inits = []
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      n = length(sort_orders[clause_ix])
      index_init = :(local $(Symbol("index_$clause_ix")) = index($(esc(clause.name)), $(sort_orders[clause_ix])))
      range_init = :(local $(Symbol("range_$(clause_ix)_0")) = 1:(length($(Symbol("index_$clause_ix"))[1])+1))
      push!(index_inits, index_init, range_init)
    end
  end  
  
  # for each var, make an Intersection
  intersection_inits = []
  for var in vars
    columns = [:($(Symbol("index_$clause_ix"))[$var_ix]) for (clause_ix, var_ix) in relation_sources[var]]
    intersection_init = :(local $(Symbol("intersection_$var")) = tuple($(columns...)))
    push!(intersection_inits, intersection_init)
  end
  
  # initialize arrays for storing results
  results_inits = []
  for (ix, typ) in enumerate(return_clause.typs)
    results_init = :(local $(Symbol("results_$ix")) = Vector{$(esc(typ))}())
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
    $([:(push!($(Symbol("results_$ix")), $(esc(var))))
    for (ix, var) in enumerate(return_clause.vars)]...)
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
    
    # run the intersection for this variable
    clause = get(var_assigned_by, var, ())
    intersection = Symbol("intersection_$var")
    range_ixes = [(clause_ix, findfirst(sort_orders[clause_ix], var_ix)) for (clause_ix, var_ix) in relation_sources[var]]
    before_ranges = Expr(:tuple, (Symbol("range_$(clause_ix)_$(range_ix-1)") for (clause_ix, range_ix) in range_ixes)...)
    after_ranges = Expr(:tuple, (Symbol("range_$(clause_ix)_$(range_ix)") for (clause_ix, range_ix) in range_ixes)...)
    if typeof(clause) == Assign
      body = (quote let
        local $(esc(var)) = $(esc(clause.expr))
        $after_ranges = project_at($intersection, $before_ranges, $(esc(var)), 0)
        if feasible_at($after_ranges)
          $body
        end
      end end).args[2]
    elseif typeof(clause) == In
      body = (quote let
        local iter = $(esc(clause.expr))
        local state = start(iter)
        local $(esc(var))
        while $need_more_results && !done(iter, state)
          ($(esc(var)), state) = next(iter, state)
          $after_ranges = project_at($intersection, $before_ranges, $(esc(var)), 0)
          if feasible_at($after_ranges)
            $body
          end
        end
      end end).args[2]
    else
      body = (quote let
        local state = $before_ranges
        local min_ix = indmin(map(length, state))
        while $need_more_results && state[min_ix].start < state[min_ix].stop
          local $(esc(var)) = $intersection[min_ix][state[min_ix].start]
          $after_ranges = project_at($intersection, state, $(esc(var)), min_ix)
          if feasible_at($after_ranges)
            $body
          end
          state = next_at(state, $after_ranges)
        end 
      end end).args[2]
    end
  end
  
  # put everything together
  results_symbols = [Symbol("results_$ix") for (ix, var) in enumerate(return_clause.vars)]
  result = :(Relation(tuple($(results_symbols...)), $(return_clause.num_keys)))        
  quote 
    let
      $(index_inits...)
      $(intersection_inits...)
      $(results_inits...)
      let 
        $body 
      end
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
