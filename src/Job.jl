module Job

using Data
using Query
using Datasets

# TODO real benchmarks
function benchmark(q, n)
  counts = 0
  results = []
  println(q, " x1 (+compilation +indexing)")
  @time begin 
    results = q()
    counts += length(results.columns[1])
  end
  println(q, " x", n)
  @time for _ in 1:n
    results = q()
    counts += length(results.columns[1])
  end
  (counts, results.columns)
end

# SELECT MIN(mc.note) AS production_note,
#        MIN(t.title) AS movie_title,
#        MIN(t.production_year) AS movie_year
# FROM company_type AS ct,
#      info_type AS it,
#      movie_companies AS mc,
#      movie_info_idx AS mi_idx,
#      title AS t
# WHERE ct.kind = 'production companies'
#   AND it.info = 'top 250 rank'
#   AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
#   AND (mc.note LIKE '%(co-production)%'
#        OR mc.note LIKE '%(presents)%')
#   AND ct.id = mc.company_type_id
#   AND t.id = mc.movie_id
#   AND t.id = mi_idx.movie_id
#   AND mc.movie_id = mi_idx.movie_id
#   AND it.id = mi_idx.info_type_id;

function q1a()
  @query([t_production_year],
  [it_info::String, it_id::Int64, mii_id::Int64, t_id::Int64, ct_id::Int64, ct_kind::String, mc_id::Int64, mc_note::String, t_production_year::Int64],
  begin 
    ct_kind = "production companies"
    it_info = "top 250 rank"
    job["company_type", "kind"](ct_id, ct_kind)
    job["info_type", "info"](it_id, it_info)
    job["movie_companies", "note"](mc_id, mc_note)
    contains(mc_note, "as Metro-Goldwyn-Mayer Pictures") == false
    (contains(mc_note, "co-production") || contains(mc_note, "presents")) == true
    job["movie_companies", "company_type_id"](mc_id, ct_id)
    job["title", "production_year"](t_id, t_production_year)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
  end)
end

# benchmark(q1a, 1000)

# SELECT MIN(t.title) AS movie_title
# FROM company_name AS cn,
#      keyword AS k,
#      movie_companies AS mc,
#      movie_keyword AS mk,
#      title AS t
# WHERE cn.country_code ='[de]'
#   AND k.keyword ='character-name-in-title'
#   AND cn.id = mc.company_id
#   AND mc.movie_id = t.id
#   AND t.id = mk.movie_id
#   AND mk.keyword_id = k.id
#   AND mc.movie_id = mk.movie_id;

function q2a()
  @query([title],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    de = "[de]"
    job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, title)
  end)
end

# benchmark(q2a, 10)

# SELECT MIN(t.title) AS movie_title
# FROM keyword AS k,
#      movie_info AS mi,
#      movie_keyword AS mk,
#      title AS t
# WHERE k.keyword LIKE '%sequel%'
#   AND mi.info IN ('Sweden',
#                   'Norway',
#                   'Germany',
#                   'Denmark',
#                   'Swedish',
#                   'Denish',
#                   'Norwegian',
#                   'German')
#   AND t.production_year > 2005
#   AND t.id = mi.movie_id
#   AND t.id = mk.movie_id
#   AND mk.movie_id = mi.movie_id
#   AND k.id = mk.keyword_id;

function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_title::String, t_production_year::Int64, mi_id::Int64, mi_info::String],
  begin 
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info", "info"](mi_id, mi_info)
    (mi_info in mi_infos) == true
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info", "movie_id"](mi_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
  end)
end

# benchmark(q3a, 20)

# SELECT MIN(mi_idx.info) AS rating,
#        MIN(t.title) AS movie_title
# FROM info_type AS it,
#      keyword AS k,
#      movie_info_idx AS mi_idx,
#      movie_keyword AS mk,
#      title AS t
# WHERE it.info ='rating'
#   AND k.keyword LIKE '%sequel%'
#   AND mi_idx.info > '5.0'
#   AND t.production_year > 2005
#   AND t.id = mi_idx.movie_id
#   AND t.id = mk.movie_id
#   AND mk.movie_id = mi_idx.movie_id
#   AND k.id = mk.keyword_id
#   AND it.id = mi_idx.info_type_id;

function q4a()
  @query([mii_info],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_production_year::Int64, it_info::String, it_id::Int64, mii_id::Int64, mii_info::String],
  begin
    job["info_type", "info"](it_id, it_info)
    it_info = "rating"
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info_idx", "info"](mii_id, mii_info)
    mii_info > "5.0"
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
  end)
end

# benchmark(q4a, 100)

end
