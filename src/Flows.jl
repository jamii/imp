module Flows

using Data
using Query

abstract Flow

type Create <: Flow
  output_name::Symbol
  keys::Vector{Type}
  vals::Vector{Type}
  transient::Bool
end

type Merge <: Flow
  output_name::Symbol
  input_names::Vector{Symbol}
  meta::Any
  eval::Function
end

type Sequence <: Flow
  flows::Vector{Flow}
end

type Fixpoint <: Flow
  flow::Flow
end

type World
  state::Dict{Symbol, Relation}
  transients::Set{Symbol}
  flow::Flow
  watchers::Set{Any}
end

function output_names(create::Create)
  Set(create.output_name)
end

function output_names(merge::Merge)
  Set(merge.output_name)
end

function output_names(sequence::Sequence)
  union(map(output_names, sequence.flows)...)
end

function output_names(fixpoint::Fixpoint)
  output_names(fixpoint.flow)
end

function (create::Create)(world::World)
  if !haskey(world.state, create.output_name) 
    output = Relation(tuple((Vector{typ}() for typ in [create.keys..., create.vals...])...), length(create.keys))
    world.state[create.output_name] = output
    if create.transient 
      push!(world.transients, create.output_name)
    end
  end
end

function (merge::Merge)(world::World)
  output = merge.eval(map((name) -> world.state[name], merge.input_names)...)
  world.state[merge.output_name] = Base.merge(world.state[merge.output_name], output)
end

function (sequence::Sequence)(world::World)
  for flow in sequence.flows
    flow(world)
  end
end

function (fixpoint::Fixpoint)(world::World)
  names = output_names(fixpoint.flow)
  while true
    old_values = map((name) -> world.state[name], names)
    fixpoint.flow(world)
    new_values = map((name) -> world.state[name], names)
    if old_values == new_values
      return
    end
  end
end

macro stateful(relation)
  (name, keys, vals) = parse_relation(relation)
  :(Create($(Expr(:quote, name)), [$(map(esc, keys)...)], [$(map(esc, vals)...)], false))
end

macro transient(relation)
  (name, keys, vals) = parse_relation(relation)
  :(Create($(Expr(:quote, name)), [$(map(esc, keys)...)], [$(map(esc, vals)...)], true))
end

macro merge(query)
  (clauses, vars, created_vars, input_names, return_clause) = Query.parse_query(query)
  code = Query.plan_query(clauses, vars, created_vars, input_names, return_clause, Set())
  escs = [:($(esc(input_name)) = $input_name) for input_name in input_names]
  code = quote
    $(escs...)
    $code
  end
  :(Merge($(Expr(:quote, return_clause.name)), $(collect(input_names)), $(Expr(:quote, query)), $(Expr(:->, Expr(:tuple, input_names...), code))))
end

function World()
  World(Dict{Symbol, Relation}(), Set{Symbol}(), Sequence([]), Set{Any}())
end

function refresh(world::World)
  old_state = copy(world.state)
  for transient in world.transients
    world.state[transient] = empty(world.state[transient])
  end
  world.flow(world)
  for watcher in world.watchers
    watcher(old_state, world.state)
  end
end

function refresh(world::World, event_table::Symbol, event_row::Tuple)
  old_state = copy(world.state)
  for transient in world.transients
    world.state[transient] = empty(world.state[transient])
  end
  push!(world.state[event_table], event_row)
  world.flow(world)
  for watcher in world.watchers
    watcher(old_state, world.state)
  end
end

function Base.getindex(world::World, name::Symbol)
  world.state[name]
end

function Base.setindex!{R <: Relation}(world::World, relation::R, name::Symbol)
  world.state[name] = relation
  refresh(world)
end

function setflow(world::World, flow::Flow)
  world.flow = flow
  refresh(world)
end

function watch(watcher, world::World)
  push!(world.watchers, watcher)
end

export Create, Merge, Sequence, Fixpoint, @stateful, @transient, @merge, World, watch, setflow, refresh

end
