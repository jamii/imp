module Layout

type Uninitialized end

isatom(val) = begin
  isbits(val) && (length(fieldnames(val)) == 0)
end

layout(val::Any) = begin
  t = typeof(val)
  fields = Tuple{Union{Symbol, Int}, Int, Any}[]
  push!(fields, (:__type__, -sizeof(Ptr), t))
  for (name, offset) in zip(fieldnames(t), fieldoffsets(t))
    field = try
      getfield(val, name)
    catch UndefRefError
      Uninitialized
    end
    push!(fields, (name, offset, field))
  end
  fields
end

layout(val::Array) = begin
  t = typeof(val)
  fields = Tuple{Union{Symbol, Int}, Int, Any}[]
  # TODO this is a lie, see https://github.com/JuliaLang/julia/blob/master/src/julia.h#L193-L229
  push!(fields, (:__type__, -sizeof(Ptr), t))
  et = eltype(val)
  if isbits(et)
    for ix in 1:length(val)
      push!(fields, (ix, (ix-1)*sizeof(et), val[ix]))
    end
  else
    for ix in 1:length(val)
      push!(fields, (ix-1)*sizeof(Ptr), val[ix])
    end
  end
  fields
end

graph(val) = begin
  seen = ObjectIdDict()
  vals = Any[val]
  while length(vals) > 0
    val = pop!(vals)
    if !isatom(val) && !haskey(seen, val) && (val != Uninitialized) && !(typeof(val) <: Type)
      println(val)
      fields = layout(val)
      seen[val] = fields
      for (_, _, child) in fields
        push!(vals, child)
      end
    end
  end
  seen
end

print_graph(val) = begin
  for (k, v) in graph(val)
    println(k, " => ", v)
  end
end

type A{T}
  a::T
  b::Int
end

print_graph(A{Vector{Tuple{Int64}}}([(1,), (2,)],0))

end