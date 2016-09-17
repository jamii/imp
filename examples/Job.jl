module Job

using Data
using Query
using JobData
using Base.Test
using BenchmarkTools

function q1a()
  @query begin 
    info_type(it_id, "top 250 rank")
    movie_info_idx(mii_id, t_id, it_id, _, _)
    title(t_id, title, _, _, production_year)
    movie_companies(mc_id, t_id, _, ct_id, note)
    company_type(ct_id, "production companies")
    @when !contains(note, "as Metro-Goldwyn-Mayer Pictures") &&
      (contains(note, "co-production") || contains(note, "presents"))
    return (note, title, production_year)
  end
end

function q2a()
  @query begin
    keyword(k_id, "character-name-in-title", _)
    movie_keyword(mk_id, t_id, k_id)
    title(t_id, title, _, _, _)
    movie_companies(mc_id, t_id, cn_id, _, _)
    company_name(cn_id, _, "[de]", _, _, _, _)
    return (title,)
  end
end

# "Denish" typo is also in original benchmarks
function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(mk_id, t_id, k_id)
    title(t_id, title, _, _, production_year)
    @when production_year > 2005
    movie_info(mi_id, t_id, _, info, _)
    @when info in infos
    return (title,)
  end
end

function q4a()
  @query begin
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(mk_id, t_id, k_id)
    title(t_id, title, _, _, production_year)
    @when production_year > 2005
    info_type(it_id, "rating")
    movie_info_idx(mi_id, t_id, it_id, info, _)
    @when info > "5.0"
    return (mi_info, title)
  end
end

function test()
  # tested against postgres
  @test length(q1a().columns[1]) == 57
  @test length(q2a().columns[1]) == 4127
  @test length(q3a().columns[1]) == 105
  @test length(q4a().columns[1]) == 45
  
  @test Base.return_types(q1a) == [Relation{Tuple{Vector{Int64}}}]
  @test Base.return_types(q2a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q3a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q4a) == [Relation{Tuple{Vector{String}}}]
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

import SQLite
function bench_sqlite()
  db = SQLite.DB("../job/job.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = 1000000000;")
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
