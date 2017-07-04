module JobData

# separate module because this takes a long time, don't want to rerun it every test

using Data
using DataFrames
using JLD

function intern{T}(column::Vector{T})
  if !isbits(T)
    interned = Dict{T, T}()
    for i in 1:length(column)
      column[i] = get!(interned, column[i], column[i])
    end
  end
end

function zero_na{T}(column::Vector{T}, na::BitVector)
  if T <: Integer
    for ix in 1:length(column)
      if na[ix]
        column[ix] = zero(T)
      end
    end
  end
end

function compress{T}(column::Vector{T}) 
  if T <: Integer
    minval, maxval = minimum(column), maximum(column)
    for T2 in [Int8, Int16, Int32, Int64]
      if (minval > typemin(T2)) && (maxval < typemax(T2))
        return convert(Vector{T2}, column)
      end
    end
  end
  return column
end

schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
table_column_names = Dict()
table_column_types = Dict()
for column in 1:size(schema)[1]
  table_name, ix, column_name, column_type = schema[column, 1:4]
  if isfile("../imdb/$(table_name).csv")
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), (column_type == "integer" ? Int64 : String))
  end
end

if !isfile("../imdb/imdb.jld")
  frames = Dict()
  @show @time for (table_name, column_names) in table_column_names
    column_types = table_column_types[table_name]
    @show table_name column_names column_types
    frame = readtable(open("../imdb/$(table_name).csv"), header=false, eltypes=column_types)
    for ix in 1:length(frame.columns)
      column = frame.columns[ix]
      intern(column.data)
      zero_na(column.data, column.na)
      frame.columns[ix] = DataArrays.DataArray(compress(column.data), column.na)
    end
    frames[table_name] = frame
  end
  @show @time save("../imdb/imdb.jld", "frames", frames)
else 
  frames = @show @time load("../imdb/imdb.jld", "frames")
  # have to intern again - not preserved by jld :(
  @show @time for frame in values(frames)
    for column in frame.columns
      intern(column.data)
    end
  end
end

function without_nulls(keys, vals, na)
  ([keys[ix] for ix in 1:length(keys) if !na[ix]],
   [vals[ix] for ix in 1:length(vals) if !na[ix]])
end

@show @time for (table_name, column_names) in table_column_names
  column_names = [replace(column_name, "_id", "") for column_name in column_names]
  frame = frames[table_name]
  typs = [Symbol("T$i") for i in 2:length(column_names)]
  fields = [Symbol(column_name) for column_name in column_names if column_name != "id"]
  relations = [:(Relation(without_nulls($(frame.columns[1].data), $(frame.columns[ix].data), $(frame.columns[ix].na)), 1)) for ix in 2:length(column_names)]
  @eval begin
    type $(Symbol("Type_$(table_name)")){$(typs...)}
      $([:($field::$typ) for (field, typ) in zip(fields, typs)]...)
    end 
    const $(Symbol(table_name)) = $(Symbol("Type_$(table_name)"))($(relations...))
    export $(Symbol(table_name))
  end
end

frames = nothing

gc()

end
