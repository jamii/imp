module Job

using Data
using Query
using JobData
using Base.Test
using BenchmarkTools

import SQLite

const q1a_sql = """
SELECT MIN(mc.note) AS production_note,
       MIN(t.title) AS movie_title,
       MIN(t.production_year) AS movie_year
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t
WHERE ct.kind = 'production companies'
  AND it.info = 'top 250 rank'
  AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
  AND (mc.note LIKE '%(co-production)%'
       OR mc.note LIKE '%(presents)%')
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND t.id = mi_idx.movie_id
  AND mc.movie_id = mi_idx.movie_id
  AND it.id = mi_idx.info_type_id;
"""

function q1a()
  q = @query begin 
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

const q2a_sql = """
SELECT MIN(t.title) AS movie_title
FROM company_name AS cn,
     keyword AS k,
     movie_companies AS mc,
     movie_keyword AS mk,
     title AS t
WHERE cn.country_code ='[de]'
  AND k.keyword ='character-name-in-title'
  AND cn.id = mc.company_id
  AND mc.movie_id = t.id
  AND t.id = mk.movie_id
  AND mk.keyword_id = k.id
  AND mc.movie_id = mk.movie_id;
"""

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

const q3a_sql = """
SELECT MIN(t.title) AS movie_title
FROM keyword AS k,
     movie_info AS mi,
     movie_keyword AS mk,
     title AS t
WHERE k.keyword LIKE '%sequel%'
  AND mi.info IN ('Sweden',
                  'Norway',
                  'Germany',
                  'Denmark',
                  'Swedish',
                  'Danish',
                  'Norwegian',
                  'German')
  AND t.production_year > 2005
  AND t.id = mi.movie_id
  AND t.id = mk.movie_id
  AND mk.movie_id = mi.movie_id
  AND k.id = mk.keyword_id;
"""

function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Danish", "Norwegian", "German"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(mk_id, t_id, k_id)
    title(t_id, title, _, _ production_year)
    @when production_year > 2005
    movie_info(mi_id, t_id, _, info, _)
    @when info in infos
    return (title,)
  end
end

const q4a_sql = """
SELECT MIN(mi_idx.info) AS rating,
       MIN(t.title) AS movie_title
FROM info_type AS it,
     keyword AS k,
     movie_info_idx AS mi_idx,
     movie_keyword AS mk,
     title AS t
WHERE it.info ='rating'
  AND k.keyword LIKE '%sequel%'
  AND mi_idx.info > '5.0'
  AND t.production_year > 2005
  AND t.id = mi_idx.movie_id
  AND t.id = mk.movie_id
  AND mk.movie_id = mi_idx.movie_id
  AND k.id = mk.keyword_id
  AND it.id = mi_idx.info_type_id;
"""

function q4a()
  @query begin
    @when contains(keyword, "sequel")
    keyword(k_id, keyword, _)
    movie_keyword(mk_id, t_id, k_id)
    title(t_id, title, _, _ production_year)
    @when production_year > 2005
    info_type(it_id, "rating")
    movie_info_idx(mi_id, t_id, it_id, info, _)
    @when info > "5.0"
    return (mii_info, title)
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

function bench()
  @show @benchmark q1a()
  @show @benchmark q2a()
  @show @benchmark q3a()
  @show @benchmark q4a()
end

function bench_sqlite()
  db = SQLite.DB("../job/job.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  SQLite.execute!(db, "ANALYZE")
  @show @benchmark SQLite.query($db, $q1a_sql)
  @show @benchmark SQLite.query($db, $q2a_sql)
  @show @benchmark SQLite.query($db, $q3a_sql)
  @show @benchmark SQLite.query($db, $q4a_sql)
end

end
