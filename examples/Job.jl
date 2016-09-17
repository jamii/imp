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

function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Danish", "Norwegian", "German"])
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

function bench()
  @show @benchmark q1a()
  @show @benchmark q2a()
  @show @benchmark q3a()
  @show @benchmark q4a()
end

end
