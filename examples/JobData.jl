module JobData

# separate module because this takes a long time, don't want to rerun it every test

using Hashed
using JLD

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict{String, Vector{String}}()
  table_column_types = Dict{String, Vector{String}}()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), column_type)
  end
  relations = Dict{String, Relation}()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      columns = Vector[]
      for (column_name, column_type) in zip(column_names, column_types)
        query = "select $column_name from $table_name"
        lines::Vector{String} = readlines(`sqlite3 ../imdb/imdb.sqlite $query`)
        if column_type == "integer"
          numbers = Int64[(line == "" || line == "\n") ? 0 : parse(Int64, line) for line in lines]
          minval = minimum(numbers)
          maxval = maximum(numbers)
          typ = first(typ for typ in [Int8, Int16, Int32, Int64] if (minval > typemin(typ)) && (maxval < typemax(typ)))
          push!(columns, convert(Vector{typ}, numbers))
        else 
          interned = Dict{String, String}()
          for ix in 1:length(lines)
            lines[ix] = get!(interned, lines[ix], lines[ix])
          end
          push!(columns, lines)
        end
      end
      relations[table_name] = Relation(tuple(columns...), 1)
    end
  end
  relations
end

if true # !isfile("../imdb/imdb_hashed.jld")
  const job = @time read_job()
  # @time save("../imdb/imdb_hashed.jld", "job", job)
else 
  const job = @time load("../imdb/imdb_hashed.jld", "job")
end

gc()

for table_name in keys(job)
  @eval begin 
    const $(Symbol(table_name)) = job[$table_name]
    export $(Symbol(table_name))
  end
end

end
