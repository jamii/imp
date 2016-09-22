module Job

using Data
using Query
using JobData
using Base.Test
using BenchmarkTools
import DataFrames
import SQLite

function q1a()
  @query begin 
    info_type(it_id, "top 250 rank")
    movie_info_idx(_, t_id, it_id, _, _)
    title(t_id, title, _, _, production_year)
    movie_companies(_, t_id, _, ct_id, note)
    company_type(ct_id, "production companies")
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)") &&
      (contains(note, "(co-production)") || contains(note, "(presents)"))
    return (note::String, title::String, production_year::Int64)
  end
end

function q2a()
  @query begin
    keyword(k_id, "character-name-in-title", _)
    movie_keyword(_, t_id, k_id)
    title(t_id, title, _, _, _)
    movie_companies(_, t_id, cn_id, _, _)
    company_name(cn_id, _, "[de]", _, _, _, _)
    return (title::String,)
  end
end

# "Denish" typo is also in original benchmarks
function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(_, t_id, k_id)
    title(t_id, title, _, _, production_year)
    @when production_year > 2005
    movie_info(_, t_id, _, info, _)
    @when info in infos
    return (title::String,)
  end
end

function q4a()
  @query begin
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(_, t_id, k_id)
    title(t_id, title, _, _, production_year)
    @when production_year > 2005
    info_type(it_id, "rating")
    movie_info_idx(_, t_id, it_id, info, _)
    @when info > "5.0"
    return (info::String, title::String)
  end
end

function bar()
  @query begin
    movie_keyword(_, t_id, _)
    title(t_id, _, _, _, _)
    @when t_id < -1
    return (t_id::Int64,)
  end
end

function test()
  @test Base.return_types(q1a) == [Relation{Tuple{Vector{String}, Vector{String}, Vector{Int64}}}]
  @test Base.return_types(q2a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q3a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q4a) == [Relation{Tuple{Vector{String}, Vector{String}}}]
  
  # SQLite does daft things with nulls - tests commented out until I can figure out how to resolve that
  # db = SQLite.DB("../imdb/imdb.sqlite")
  # for q in 1:4
  #   results_imp = eval(Symbol("q$(q)a"))()
  #   query = rstrip(readline("../job/$(q)a.sql"))
  #   query = replace(query, "MIN", "")
  #   frame = SQLite.query(db, query)
  #   num_columns = length(results_imp.columns)
  #   results_sqlite = Relation(tuple((frame[ix].values for ix in 1:num_columns)...), num_columns)
  #   (imp_only, sqlite_only) = Data.diff(results_imp, results_sqlite)
  #   @show q 
  #   @test imp_only.columns == sqlite_only.columns # ie both empty - but @test will print both otherwise
  # end
  
  for q in 1:4
    results_imp = eval(Symbol("q$(q)a"))()
    query = rstrip(readline("../job/$(q)a.sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    query = "copy ($query) to '/tmp/results.csv' with CSV DELIMITER ',';"
    run(`sudo -u postgres psql -c $query`)
    frame = DataFrames.readtable(open("/tmp/results.csv"), header=false, eltypes=[eltype(c) for c in results_imp.columns])
    num_columns = length(results_imp.columns)
    results_pg = Relation(tuple((frame[ix].data for ix in 1:num_columns)...), num_columns)
    (imp_only, pg_only) = Data.diff(results_imp, results_pg)
    @show q 
    @test imp_only.columns == pg_only.columns # ie both empty - but @test will print both otherwise
  end
end

function bench_imp()
  medians = []
  for q in 1:4
    @show q
    trial = @show @benchmark $(eval(Symbol("q$(q)a")))()
    push!(medians, @show (median(trial.times) / 1000000))
  end
  medians
end

function bench_sqlite()
  db = SQLite.DB("../imdb/imdb.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  medians = []
  for q in 1:4
    query = rstrip(readline("../job/$(q)a.sql"))
    @time SQLite.query(db, query)
    trial = @show @benchmark SQLite.query($db, $query)
    push!(medians, @show (median(trial.times) / 1000000))
  end
  medians
end

function bench_pg()
  medians = []
  for q in 1:4
    query = rstrip(readline("../job/$(q)a.sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    bench = "explain analyze $query"
    cmd = `sudo -u postgres psql -c $bench`
    times = Float64[]
    @show q
    @show @benchmark push!($times, parse(Float64, match(r"Execution time: (\S*) ms", readstring($cmd))[1]))
    push!(medians, @show median(times))
  end
  medians
end

end
