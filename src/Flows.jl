module Flows

using Data
using Query

type Union <: AbstractView
  views::Vector{AbstractView}
end

function (union::Union)(inputs::Dict{Symbol, Relation})
  reduce(merge, (view(inputs) for view in union.views))
end

type Flow
  inputs::Dict{Symbol, Relation}
  views::Vector{Pair{Symbol, AbstractView}} # TODO make views a dict, do topo sort
  outputs::Dict{Symbol, Relation}
  watchers::Set{Any}
end

function Flow()
  Flow(Dict{Symbol, Relation}(), Vector{Pair{Symbol, AbstractView}}(), Dict{Symbol, Relation}(), Set{Any}())
end

function refresh(flow::Flow)
  old_outputs = flow.outputs
  new_outputs = copy(flow.inputs)
  for (name, view) in flow.views
    new_outputs[name] = view(new_outputs)
  end
  flow.new_outputs = new_outputs
  for watcher in flow.watchers
    watcher(old_outputs, new_outputs)
  end
end

function Base.getindex(flow::Flow, name::Symbol)
  flow.outputs[name]
end

function Base.setindex!(flow::Flow, input::Relation, name::Symbol)
  flow.inputs[name] = input
  refresh(flow)
end

function setviews(flow::Flow, views::Vector{Pair{Symbol, AbstractView}})
  flow.views = views
  refresh(flow)
end

function watch(watcher, flow::Flow)
  push!(flow.watchers, watcher)
end

export Flow, setviews

end
