module Flows

using Data
using Query
using Match

abstract Flow

type Create <: Flow
  output_name::Symbol
  keys::Vector{Type}
  vals::Vector{Type}
  empty::Relation
  is_transient::Bool
  is_event::Bool
end

function Create(output_name, keys, vals, is_transient, is_event)
  empty = Relation(tuple((Data.column_type(typ)() for typ in [keys..., vals...])...), length(keys))
  Create(output_name, keys, vals, empty, is_transient, is_event)
end

type Merge <: Flow
  output_names::Vector{Symbol}
  input_names::Vector{Symbol}
  meta::Any
  eval::Function
end

type Sequence <: Flow
  flows::Vector{Flow}
end

type Fixpoint <: Flow
  flow::Flow
  output_names::Vector{Symbol}
end

function Fixpoint(flow::Flow)
  Fixpoint(flow, collect(output_names(flow)))
end

type World
  state::Dict{Symbol, Relation}
  events::Set{Symbol}
  flow::Flow
end

function output_names(create::Create)
  Set([create.output_name])
end

function output_names(merge::Merge)
  Set(merge.output_names)
end

function output_names(sequence::Sequence)
  union(map(output_names, sequence.flows)...)
end

function output_names(fixpoint::Fixpoint)
  output_names(fixpoint.flow)
end

function init_flow(_, world::World)
  # do nothing by default
end

function init_flow(flow::Create, world::World)
  if flow.is_transient || !haskey(world.state, flow.output_name) 
    world.state[flow.output_name] = copy(flow.empty)
  end
  if flow.is_event
    push!(world.events, flow.output_name)
  end
end

function init_flow(flow::Sequence, world::World)
  for child in flow.flows
    init_flow(child, world)
  end
end

function init_flow(flow::Fixpoint, world::World)
  init_flow(flow.flow, world)
end

function run_flow(_, world::World)
  # do nothing by default
end

function run_flow(flow::Merge, world::World)
  outputs::Vector{Relation} = flow.eval(map((name) -> world.state[name], flow.input_names)...)
  for (output_name, output) in zip(flow.output_names, outputs)
    world.state[output_name] = Base.merge(world.state[output_name], output)
  end
end

function run_flow(flow::Sequence, world::World)
  for child in flow.flows
    run_flow(child, world)
  end
end

function run_flow(flow::Fixpoint, world::World)
  while true
    old_values = map((name) -> world.state[name].columns, flow.output_names)
    run_flow(flow.flow, world)
    new_values = map((name) -> world.state[name].columns, flow.output_names)
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

function World()
  World(Dict{Symbol, Relation}(), Set{Symbol}(), Sequence([]))
end

function refresh(world::World)
  old_state = copy(world.state)
  init_flow(world.flow, world)
  run_flow(world.flow, world)
  (old_state, world.state)
end

function refresh(world::World, event_table::Symbol, event_row::Tuple)
  @show :event event_table event_row
  old_state = copy(world.state)
  @show @time init_flow(world.flow, world)
  @show @time push!(world.state[event_table], event_row)
  @show @time run_flow(world.flow, world)
  (old_state, world.state)
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

export Flow, Create, Merge, Sequence, Fixpoint, @stateful, @transient, @event, @merge, World, set_flow!, refresh

end
