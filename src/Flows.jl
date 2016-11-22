module Flows

using Data
using Query

type Flow
  relations::Dict{Symbol, Relation}
  views::Vector{Pair{Symbol, View}} # TODO make views a dict, do topo sort
  cached::Dict{Symbol, Relation}
  watchers::Set{Any}
end

function Flow()
  Flow(Dict{Symbol, Relation}(), Vector{Pair{Symbol, View}}(), Dict{Symbol, Relation}(), Set{Any}())
end

function refresh(flow::Flow)
  old_cached = flow.cached
  cached = copy(flow.relations)
  for (name, view) in flow.views
    cached[name] = view(cached)
  end
  flow.cached = cached
  for watcher in flow.watchers
    watcher(old_cached, cached)
  end
end

function Base.getindex(flow::Flow, name::Symbol)
  flow.cached[name]
end

function Base.setindex!(flow::Flow, relation::Relation, name::Symbol)
  flow.relations[name] = relation
  refresh(flow)
end

function setviews(flow::Flow, views::Vector{Pair{Symbol, View}})
  flow.views = views
  refresh(flow)
end

function watch(watcher, flow::Flow)
  push!(flow.watchers, watcher)
end

export Flow, setviews

end
