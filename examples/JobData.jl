module JobData

# separate module because this takes a long time, don't want to rerun it every test

using Hashed

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict{String, Vector{String}}()
  table_column_types = Dict{String, Vector{String}}()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), column_type)
  end
  tables = Dict{String, Tuple}()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      columns = Vector[]
      for (column_name, column_type) in zip(column_names, column_types)
        query = "select $column_name from $table_name"
        lines::Vector{SubString{String}} = split(readstring(`sqlite3 ../imdb/imdb.sqlite $query`), '\n')
        if column_type == "integer"
          numbers = Int64[(line == "") ? 0 : parse(Int64, line) for line in lines]
          minval = minimum(numbers)
          maxval = maximum(numbers)
          typ = first(typ for typ in [Int8, Int16, Int32, Int64] if (minval > typemin(typ)) && (maxval < typemax(typ)))
          push!(columns, convert(Vector{typ}, numbers))
        else 
          interned = Dict{SubString{String}, String}()
          for ix in 1:length(lines)
            lines[ix] = get!(() -> string(lines[ix]), interned, lines[ix])
          end
          push!(columns, lines)
        end
      end
      tables[table_name] = tuple(columns...)
    end
  end
  tables
end

job = @time read_job()

if !isfile("../imdb/imdb.imp")
  job = @time read_job()
  @time serialize(open("../imdb/imdb.imp", "w"), job)
else 
  job = @time deserialize(open("../imdb/imdb.imp"))
end

gc()

for table_name in keys(job)
  @eval begin 
    const $(Symbol(table_name)) = Relation(job[$table_name], 1)
    export $(Symbol(table_name))
  end
end

end
