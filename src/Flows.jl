module Flows

using Data
using Query
using Match

abstract Flow

type Create <: Flow
  output_name::Symbol
  keys::Vector{Type}
  vals::Vector{Type}
  is_transient::Bool
  is_event::Bool
end

type Merge <: Flow
  output_names::Vector{Symbol}
  input_names::Vector{Symbol}
  meta::Any
  eval::Function
end

type Clear <: Flow
  output_name::Symbol
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
  events::Set{Symbol}
  flow::Flow
  watchers::Set{Any}
end

function output_names(create::Create)
  Set([create.output_name])
end

function output_names(merge::Merge)
  Set(merge.output_names)
end

function output_names(clear::Clear)
  Set([clear.output_name])
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
    if create.is_transient 
      push!(world.transients, create.output_name)
    end
    if create.is_event
      push!(world.events, create.output_name)
    end
  end
end

function (merge::Merge)(world::World)
  outputs = merge.eval(map((name) -> world.state[name], merge.input_names)...)
  for (output_name, output) in zip(merge.output_names, outputs)
    world.state[output_name] = Base.merge(world.state[output_name], output)
  end
end

function (clear::Clear)(world::World)
  input = world.state[clear.output_name]
  output = Relation(tuple((Vector{eltype(c)}() for c in input.columns)...), input.num_keys)
  world.state[clear.output_name] = output
end

function (sequence::Sequence)(world::World)
  for flow in sequence.flows
    # @show flow
    flow(world)
  end
end

function (fixpoint::Fixpoint)(world::World)
  names = collect(output_names(fixpoint.flow))
  while true
    old_values = map((name) -> world.state[name].columns, names)
    fixpoint.flow(world)
    new_values = map((name) -> world.state[name].columns, names)
    @show old_values new_values
    if old_values == new_values
      return
    end
  end
end

function get_type(expr) 
  @match expr begin
    Expr(:(::), [_, typ], _) => typ
    _ => expr
  end
end

macro stateful(relation)
  (name, keys, vals) = parse_relation(relation)
  key_types = map(get_type, keys)
  val_types = map(get_type, vals)
  :(Create($(Expr(:quote, name)), [$(map(esc, key_types)...)], [$(map(esc, val_types)...)], false, false))
end

macro transient(relation)
  (name, keys, vals) = parse_relation(relation)
  key_types = map(get_type, keys)
  val_types = map(get_type, vals)
  :(Create($(Expr(:quote, name)), [$(map(esc, key_types)...)], [$(map(esc, val_types)...)], true, false))
end

macro event(relation)
  (name, keys, vals) = parse_relation(relation)
  key_types = map(get_type, keys)
  val_types = map(get_type, vals)
  :(Create($(Expr(:quote, name)), [$(map(esc, key_types)...)], [$(map(esc, val_types)...)], true, true))
end

macro merge(query)
  (clauses, vars, created_vars, input_names, return_clauses) = Query.parse_query(query)
  code = Query.plan_query(clauses, vars, created_vars, input_names, return_clauses, Set())
  escs = [:($(esc(input_name)) = $input_name) for input_name in input_names]
  for return_clause in return_clauses
    @assert isa(return_clause.name, Symbol)
  end
  code = quote
    $(escs...)
    $code
  end
  :(Merge($([return_clause.name for return_clause in return_clauses]), $(collect(input_names)), $(Expr(:quote, query)), $(Expr(:->, Expr(:tuple, input_names...), code))))
end

macro clear(name)
  :(Clear($(Expr(:quote, name))))
end

function World()
  World(Dict{Symbol, Relation}(), Set{Symbol}(),Set{Symbol}(), Sequence([]), Set{Any}())
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
  @show :event event_table event_row
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

function set_flow!(world::World, flow::Flow)
  world.flow = flow
  refresh(world)
end

function watch(watcher, world::World)
  push!(world.watchers, watcher)
end

export Create, Merge, Sequence, Fixpoint, @stateful, @transient, @event, @merge, @clear, World, watch, set_flow!, refresh

end
