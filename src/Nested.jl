module Nested

type Relation{T}
  unique::Any
  indexes::Dict{Vector{Int64}, Any}
end

function typeof_index(column_types)
  typs = Type[Void]
  for i in length(column_types):-1:1
    insert!(typs, 1, Dict{column_types[i], typs[1]})
  end
  typs
end

function typeof_index(column_types, order)
  typeof_index([column_types[ix] for ix in order])
end

function typeof_columns(data_types)
  typs = []
  @assert data_types.name == Tuple.name
  for data_type in data_types.parameters
    @assert data_type.name == Array.name
    @assert data_type.parameters[2] == 1
    push!(typs, data_type.parameters[1])
  end
  typs
end

@generated function push_index!{order}(index_1, columns::Tuple, ::Type{Val{order}})
  column_types = typeof_columns(columns)
  index_types = typeof_index(column_types, order)
  index_gets = map(1:length(column_types)) do ix
    :($(Symbol("index_$(ix+1)")) = get!($(index_types[ix+1]), $(Symbol("index_$ix")), columns[$(order[ix])][i]))
  end
  quote
    for i in 1:length(columns[1])
      $(index_gets...)
    end
  end
end

function Base.merge!{T}(relation::Relation{T}, columns::Tuple)
  for (order, index) in relation.indexes
    push_index!(index, columns, Val{tuple(order...)})
  end
end

function Relation(columns::Tuple, num_keys::Int64)
  column_types = typeof_columns(typeof(columns))
  order = collect(1:length(column_types))
  index = typeof_index(column_types)[1]()
  indexes = Dict{Vector{Int64}, Any}(order => index)
  relation = Relation{Tuple{column_types...}}(index, indexes)
  merge!(relation, columns)
  relation
end

@generated function reshape_index!{T, order}(::Type{T}, from_index_1, to_index_1, ::Type{Val{order}})
  n = length(T.parameters)
  index_types = typeof_index(T.parameters, order)
  index_gets = map(1:n) do ix
    :($(Symbol("to_index_$(ix+1)")) = get!($(index_types[ix+1]), $(Symbol("to_index_$ix")), $(Symbol("val_$(order[ix])"))))
  end
  body = quote $(index_gets...) end
  for i in n:-1:1
    body = quote
      foreach($(Symbol("from_index_$i"))) do pair
        ($(Symbol("val_$i")), $(Symbol("from_index_$(i+1)"))) = pair
        $body
      end
    end
  end
  quote
    $body
  end
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  get!(relation.indexes, order) do
    index = typeof_index(T.parameters, order)[1]()
    reshape_index!(T, relation.unique, index, Val{tuple(order...)})
    index
  end
end

@generated function index{T, order}(relation::Relation{T}, ::Type{Val{order}})
  typ = typeof_index(T.parameters, order)[1]
  quote
    $(Expr(:meta, :inline))
    index(relation, [$order...])::$typ
  end
end
  
type Finger{I}
  index::I
  state::Int64
end

@inline function finger(relation::Relation, index)
  Finger(index, -1)
end

@inline function finger{K,V}(relation::Relation, index, finger::Finger{Dict{K,V}}, col_ix)
  Finger(V(), -1)
end

@inline function Base.length(finger::Finger)
  length(finger.index)
end

@inline function project(finger::Finger, down_finger::Finger, val)
  down_finger.index = get(finger.index, val, down_finger.index)
  haskey(finger.index, val)
end
  
@inline function Base.start(finger::Finger, down_finger::Finger)
  down_finger.state = start(finger.index)
  down_finger.index = get(finger.index.vals, down_finger.state, down_finger.index)
  !done(finger.index, down_finger.state)
end

@inline function Base.next(finger::Finger, down_finger::Finger)
  down_finger.state = Base.skip_deleted(finger.index, down_finger.state+1)
  down_finger.index = get(finger.index.vals, down_finger.state, down_finger.index)
  !done(finger.index, down_finger.state)
end

@inline function head(finger::Finger, down_finger::Finger)
  finger.index.keys[down_finger.state]
end

export Relation, index, Finger, finger, project, head

end
