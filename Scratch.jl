module Scratch

using Data
using Query
using Datasets

# srand(999)
# # edge = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
# edge = Relation(([1, 2, 3, 3, 4], [2, 3, 1, 4, 2]))
# # edge = read_columns("/home/jamie/soc-LiveJournal1.txt", [Int32, Int32], comments=true)
# 
# function f(edge) 
#   @join([a,b,c], [a::Int64,b::Int64,c::Int64], 
#   begin
#     edge(a,b)
#     a < b
#     edge(b,c)
#     b < c
#     edge(c,a)
#   end)
# end
# 
# macroexpand(quote 
# @join([a,b,c], [a::Int64,b::Int64,c::Int64], 
# begin
#   edge(a,b)
#   a < b
#   edge(b,c)
#   b < c
#   edge(c,a)
# end)
# end)
# 
# # @code_warntype f(edge)
# f(edge)
# f(edge)
# 
# macroexpand(quote
# @join([an],
# [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
# begin
#   pn = "Heavy Metal Classic"
#   playlist(p, pn)
#   playlist_track(p, t)
#   track(t, _, al)
#   album(al, _, a)
#   artist(a, an)
# end)
# end)
# 
# function who_is_metal(album, artist, track, playlist_track, playlist)
#   metal = @join([an],
#   [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
#   begin
#     pn = "Heavy Metal Classic"
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al)
#     album(al, _, a)
#     artist(a, an)
#   end)
#   @join([an],
#   [an::String], 
#   begin
#     metal(an)
#   end)
# end
# 
# macroexpand(quote
# metal = @join([an],
# [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
# begin
#   pn = "Heavy Metal Classic"
#   playlist(p, pn)
#   playlist_track(p, t)
#   track(t, _, al)
#   album(al, _, a)
#   artist(a, an)
# end)
# @join([an],
# [an::String], 
# begin
#   metal(an)
# end)
# end)
# 
# function who_is_metal2(album, artist, track, playlist_track, playlist)
#   i1 = @join([p],
#   [pn::String, p::Int64],
#   begin
#     pn = "Heavy Metal Classic"
#     playlist(p, pn)
#   end)
#   i2 = @join([t],
#   [p::Int64, t::Int64],
#   begin 
#     i1(p)
#     playlist_track(p, t)
#   end)
#   i3 = @join([al],
#   [t::Int64, al::Int64],
#   begin 
#     i2(t)
#     track(t, _, al)
#   end)
#   i4 = @join([a],
#   [al::Int64, a::Int64],
#   begin 
#     i3(al)
#     album(al, _, a)
#   end)
#   i5 = @join([an],
#   [a::Int64, an::String],
#   begin 
#     i4(a)
#     artist(a, an)
#   end)
#   @join([an],
#   [an::String],
#   begin
#     i5(an)
#   end)
# end
# 
# who_is_metal(album, artist, track, playlist_track, playlist)
# who_is_metal2(album, artist, track, playlist_track, playlist)
# 
# # assert(who_is_metal(album, artist, track, playlist_track, playlist) == who_is_metal2(album, artist, track, playlist_track, playlist))
# 
# # @code_warntype who_is_metal(album, artist, track, playlist_track, playlist)
# 
# # @time [who_is_metal(album, artist, track, playlist_track, playlist) for n in 1:10000]
# # @time [who_is_metal2(album, artist, track, playlist_track, playlist) for n in 1:10000]
# 
# function how_metal(album, artist, track, playlist_track, playlist)
#   metal = @join([],
#   [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
#   begin
#     pn = "Heavy Metal Classic"
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al)
#     album(al, _, a)
#     artist(a, an)
#   end)
# end
# 
# @time how_metal(album, artist, track, playlist_track, playlist)
# 
# function cost_of_playlist(album, artist, track, playlist_track, playlist)
#   @join([pn],
#   [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
#   (0.0,add_exp,price::Float64),
#   begin
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al, _, _, _, _, _, price)
#   end)
# end
# 
# @time cost_of_playlist(album, artist, track, playlist_track, playlist)
# 
# function revenue_per_track(album, artist, track, playlist_track, playlist)
#   result = @join([p, pn, t],
#   [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
#   (0.0,add_exp,price::Float64),
#   begin
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al, _, _, _, _, _, price)
#   end)
#   @join([t],
#   [t::Int64, p::Int64, pn::String, price::Float64],
#   (0.0,add_exp,price::Float64),
#   begin
#     result(p, pn, t, price)
#   end)
# end
# 
# function revenue_per_track2(album, artist, track, playlist_track, playlist)
#   @query([t],
#   [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
#   (0.0,add_exp,price::Float64),
#   begin
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al, _, _, _, _, _, price)
#   end)
# end
# 
# @time revenue_per_track(album, artist, track, playlist_track, playlist)
# @time revenue_per_track2(album, artist, track, playlist_track, playlist)
# 
# assert(revenue_per_track(album, artist, track, playlist_track, playlist).columns == revenue_per_track2(album, artist, track, playlist_track, playlist).columns)
# 
# @query([pn, an],
# [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
# begin
#   playlist(p, pn)
#   playlist_track(p, t)
#   track(t, _, al)
#   album(al, _, a)
#   artist(a, an)
# end).columns

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
    ismatch(r".*as Metro-Goldwyn-Mayer Pictures.*", mc_note) == false
    (ismatch(r".*co-production.*", mc_note) || ismatch(r".*presents.*", mc_note)) == true
    job["movie_companies", "company_type_id"](mc_id, ct_id)
    job["title", "production_year"](t_id, t_production_year)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
  end)
end

@time q1a()

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

@time q2a()

end
