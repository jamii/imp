module JobData

# separate module because this takes a long time, don't want to rerun it every test

using Hashed
using DataFrames
using JLD

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict()
  table_column_types = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), (column_type == "integer" ? Int64 : String))
  end
  relations = Dict()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      frame = readtable(open("../imdb/$(table_name).csv"), header=false, eltypes=column_types)
      columns = tuple((frame[ix].data for ix in 1:length(column_names))...)
      relations[table_name] = Relation(columns, 1)
    end
  end
  relations
end

if true # !isfile("../imdb/imdb_hashed.jld")
  job = @time read_job()
  # @time save("../imdb/imdb_hashed.jld", "job", job)
else 
  job = @time load("../imdb/imdb_hashed.jld", "job")
end

gc()

for table_name in keys(job)
  @eval begin 
    const $(Symbol(table_name)) = job[$table_name]
    export $(Symbol(table_name))
  end
end

end
