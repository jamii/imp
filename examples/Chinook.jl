module Scratch

using Data
using Query
using Datasets

srand(999)
# edge = (rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6)))
edge = Relation(([1, 2, 3, 3, 4], [2, 3, 1, 4, 2]))
# edge = read_columns("/home/jamie/soc-LiveJournal1.txt", [Int32, Int32], comments=true)

function f(edge) 
  @query([a,b,c], [a::Int64,b::Int64,c::Int64], 
  begin
    edge(a,b)
    a < b
    edge(b,c)
    b < c
    edge(c,a)
  end)
end

results = @query([a::Int64,b::Int64,c::Int64], 
begin
  edge(a,b)
  @when a < b
  edge(b,c)
  @when b < c
  edge(c,a)
end)

# @code_warntype f(edge)
f(edge)
f(edge)

@query([artist_name::String],
begin
  playlist_name(playlist, "Heavy Metal Classic")
  playlist_track(playlist, track)
  track_album(track, album)
  album_artist(album, artist)
  artist_name(artist, artist_name)
end)

function who_is_metal(album, artist, track, playlist_track, playlist)
  metal = @join([an],
  [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end)
  @join([an],
  [an::String], 
  begin
    metal(an)
  end)
end

macroexpand(quote
metal = @join([an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end)
@join([an],
[an::String], 
begin
  metal(an)
end)
end)

function who_is_metal2(album, artist, track, playlist_track, playlist)
  i1 = @join([p],
  [pn::String, p::Int64],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
  end)
  i2 = @join([t],
  [p::Int64, t::Int64],
  begin 
    i1(p)
    playlist_track(p, t)
  end)
  i3 = @join([al],
  [t::Int64, al::Int64],
  begin 
    i2(t)
    track(t, _, al)
  end)
  i4 = @join([a],
  [al::Int64, a::Int64],
  begin 
    i3(al)
    album(al, _, a)
  end)
  i5 = @join([an],
  [a::Int64, an::String],
  begin 
    i4(a)
    artist(a, an)
  end)
  @join([an],
  [an::String],
  begin
    i5(an)
  end)
end

who_is_metal(album, artist, track, playlist_track, playlist)
who_is_metal2(album, artist, track, playlist_track, playlist)

# assert(who_is_metal(album, artist, track, playlist_track, playlist) == who_is_metal2(album, artist, track, playlist_track, playlist))

# @code_warntype who_is_metal(album, artist, track, playlist_track, playlist)

# @time [who_is_metal(album, artist, track, playlist_track, playlist) for n in 1:10000]
# @time [who_is_metal2(album, artist, track, playlist_track, playlist) for n in 1:10000]

function how_metal(album, artist, track, playlist_track, playlist)
  metal = @join([],
  [pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end)
end

@time how_metal(album, artist, track, playlist_track, playlist)

function cost_of_playlist(album, artist, track, playlist_track, playlist)
  @join([pn],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
end

@time cost_of_playlist(album, artist, track, playlist_track, playlist)

function revenue_per_track(album, artist, track, playlist_track, playlist)
  result = @join([p, pn, t],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
  @join([t],
  [t::Int64, p::Int64, pn::String, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    result(p, pn, t, price)
  end)
end

function revenue_per_track2(album, artist, track, playlist_track, playlist)
  @query([t],
  [p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
  (0.0,add_exp,price::Float64),
  begin
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al, _, _, _, _, _, price)
  end)
end

@time revenue_per_track(album, artist, track, playlist_track, playlist)
@time revenue_per_track2(album, artist, track, playlist_track, playlist)

assert(revenue_per_track(album, artist, track, playlist_track, playlist).columns == revenue_per_track2(album, artist, track, playlist_track, playlist).columns)

@query([pn, an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end).columns

end
