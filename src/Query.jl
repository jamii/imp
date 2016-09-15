module Query

using Data
using Match

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
      if los[ix+1] >= his[ix+1]
        return false
      end
    end
    return true
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

function get_var_symbol(expr)
  if isa(expr, Expr) && expr.head == :(::)
    expr.args[1]
  else 
    expr
  end
end

function get_var_type(expr)
  if isa(expr, Expr) && expr.head == :(::)
    expr.args[2]
  else 
    Any
  end
end

function random_value_from(columns)
  columns[rand(1:length(columns))][1]
end

function random_value_from(columns, value) 
  rand(Bool) ? value : random_value_from(columns)
end

type Row; name; vars; num_keys; end
type When; expr; vars; end
type Assign; var; expr; vars; end
type In; var; expr; vars; end
type Hint; var; end
type Return; name; vars; num_keys; end

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
        Return(name, Any[keys..., vals...], length(keys)) 
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
    return_clause = Return((), vars, length(vars))
  elseif length(returns) == 1
    return_clause = returns[1]
  else
    error("Too many returns: $returns")
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
  sources = Dict(var => Tuple{Int64, Int64}[] for var in vars)
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      for (var_ix, var) in enumerate(clause.vars)
        if var != :_
          push!(sources[var], (clause_ix, var_ix))
        end
      end
    end
  end
  
  # for each Row clause, figure out what order to sort the index in
  sort_orders = Dict(clause_ix => Int64[] for clause_ix in 1:length(clauses))
  for var in vars 
    for (clause_ix, var_ix) in sources[var]
      push!(sort_orders[clause_ix], var_ix)
    end
  end
  
  # assign a slot in the los/ats/his arrays for each relation/column pair
  ixes = Tuple{Int64, Any}[]
  for (clause_ix, var_ixes) in sort_orders
    for var_ix in var_ixes
      push!(ixes, (clause_ix, var_ix))
    end
    push!(ixes, (clause_ix, :buffer))
  end
  ix_for = Dict(column => ix for (ix, column) in enumerate(ixes))
  
  # --- codegen ---
  
  # for each Row clause, get the correct index
  index_inits = []
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      order = sort_orders[clause_ix]
      index_init = :($(Symbol("index_$clause_ix")) = index($(Symbol("relation_$clause_ix")), $order))
      push!(index_inits, index_init)
    end
  end
  
  # for each var, collect up the columns to be intersected
  columns_inits = []
  for var in vars
    columns = [:($(Symbol("index_$clause_ix"))[$var_ix]) for (clause_ix, var_ix) in sources[var]]
    columns_init = :($(Symbol("columns_$var")) = [$(columns...)])
    push!(columns_inits, columns_init)
  end
  
  # for each var, make list of ixes into the global state
  ixes_inits = []
  for var in vars 
    ixes_init = :($(Symbol("ixes_$var")) = $(Int64[ix_for[source] for source in sources[var]]))
    push!(ixes_inits, ixes_init)
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
  for var in vars    
    inputs = [:($(Symbol("index_$clause_ix"))[$var_ix][1]) for (clause_ix, var_ix) in sources[var]]
    if haskey(var_assigned_by, var)
      clause = var_assigned_by[var]
      (args, _) = eval_funs[var]
      inferred_args = [:($(Symbol("results_$arg"))[1]) for arg in args]
      eval_call = :($(esc(Symbol("eval_$var")))($(inferred_args...)))
      if typeof(clause) == In
        eval_call = :(first($eval_call))
      end
      push!(inputs, eval_call)
    end
    results_init = :($(Symbol("results_$var")) = [[$(inputs...)][rand(Int64)] for _ in []])
    push!(results_inits, results_init)
  end
  
  # initilize global state
  los = [1 for _ in ixes]
  ats = [1 for _ in ixes]
  his = []
  for (clause_ix, var_ix) in ixes
    if var_ix == :buffer
      push!(his, 0)
    else 
      push!(his, :(length($(Symbol("index_$clause_ix"))[$var_ix]) + 1))
    end
  end
  
  # combine all the init steps
  init = quote
    $(index_inits...)
    $(columns_inits...)
    $(ixes_inits...)
    $((eval_fun for (var, (args, eval_fun)) in eval_funs)...)
    $((when_fun for (var, (args, when_fun)) in when_funs)...)
    $(results_inits...)
    los = [$(los...)]
    ats = [$(ats...)]
    his = [$(his...)]
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
    var_columns = Symbol("columns_$var")
    var_ixes = Symbol("ixes_$var")
    
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
    if typeof(clause) == Assign
      body = quote
        let $var = $eval_call
          if assign($var_columns, los, ats, his, $var_ixes, $var)
            $body
          end
        end
      end
    elseif typeof(clause) == In
      body = quote 
        let
          local iter = $eval_call
          local state = start(iter)
          local $var
          while $need_more_results && !done(iter, state)
            ($var, state) = next(iter, state)
            if assign($var_columns, los, ats, his, $var_ixes, $var)
              $body
            end
          end
        end
      end
    else 
      result_column = ix_for[sources[var][1]]
      body = quote
        start_intersect($var_columns, los, ats, his, $var_ixes)
        while $need_more_results && next_intersect($var_columns, los, ats, his, $var_ixes)
          let $var = $(Symbol("columns_$var"))[1][los[$(result_column+1)]]
            $body
          end
        end
      end 
    end
    
  end
  
  query_symbol = gensym("query")
  relation_symbols = [Symbol("relation_$clause_ix") for (clause_ix, clause) in enumerate(clauses) if typeof(clause) == Row]
  relation_names = [esc(clause.name) for clause in clauses if typeof(clause) == Row]
  results_symbols = [Symbol("results_$var") for var in return_clause.vars]
          
  quote 
    function $query_symbol($(relation_symbols...))
      $init
      $body
      Relation(tuple($(results_symbols...)), $(return_clause.num_keys))
    end
    result = $query_symbol($(relation_names...))
    $(if return_clause.name != ()
      :(merge!($(esc(return_clause.name)), result))
    else
      :result
    end) 
  end
end

macro query(query)
  plan_query(query)
end

export @query

end
