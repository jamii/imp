module Chinook 

using Data
using Query
using BenchmarkTools
using Base.Test

function read_chinook(filename, types; comments=false)
  rows, _ = readdlm(open(filename), '\t', header=true, quotes=false, comments=comments)
  n = length(types)
  columns = tuple([Vector{types[c]}() for c in 1:n]...)
  for r in 1:size(rows)[1]
    for c in 1:n
      if types[c] == String
        push!(columns[c], string(rows[r, c]))
      else
        push!(columns[c], rows[r, c])
      end
    end
  end
  Relation(columns)
end

album = read_chinook("data/Album.csv", [Int64, String, Int64])
artist = read_chinook("data/Artist.csv", [Int64, String])
track = read_chinook("data/Track.csv", [Int64, String, Int64, Int64, Int64, String, Int64, Int64, Float64])
playlist_track = read_chinook("data/PlaylistTrack.csv", [Int64, Int64])
playlist = read_chinook("data/Playlist.csv", [Int64, String])

function who_is_metal()
  @query([artist_name::String],
  begin
    playlist(playlist, "Heavy Metal Classic")
    playlist_track(playlist, track)
    track(track, _, album)
    album(album, _, artist)
    artist(artist, artist_name)
  end)
end

# function how_metal()
#   metal = @query([],
#   begin
#     playlist(playlist, "Heavy Metal Classic")
#     playlist_track(playlist, track)
#     track(track, _, album)
#     album(album, _, artist)
#     artist(artist, artist_name)
#   end)
# end

# function cost_of_playlist()
#   @query([pn::String],
#   (0.0,add_exp,price::Float64),
#   begin
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al, _, _, _, _, _, price)
#   end)
# end

# function revenue_per_track()
#   @query([t::Int64],
#   (0.0,add_exp,price::Float64),
#   begin
#     playlist(p, pn)
#     playlist_track(p, t)
#     track(t, _, al, _, _, _, _, _, price)
#   end)
# end

function test()
  @test who_is_metal().columns == (
  String["AC/DC","Accept","Black Sabbath","Iron Maiden","Metallica","Motörhead","Mötley Crüe","Ozzy Osbourne","Scorpions"],
  )
  # @test how_metal().columns == (
  # [26],
  # )
  # @test cost_of_playlist().columns == (
  # String["90’s Music","Brazilian Music","Classical","Classical 101 - Deep Cuts","Classical 101 - Next Steps","Classical 101 - The Basics","Grunge","Heavy Metal Classic","Music","Music Videos","On-The-Go 1","TV Shows"],
  # Float64[1462.2300000000118,38.60999999999999,74.24999999999999,24.74999999999999,24.74999999999999,24.74999999999999,14.850000000000001,25.739999999999988,6514.199999999501,0.99,0.99,847.7400000000024]
  # )
  # @test revenue_per_track().columns[1][1:10] == Int64[1,2,3,4,5,6,7,8,9,10]
  # @test revenue_per_track().columns[2][1:10] == Float64[2.9699999999999998,2.9699999999999998,3.96,3.96,3.96,1.98,1.98,1.98,1.98,1.98]
end

function bench()
  @show @benchmark who_is_metal()
  # @show @benchmark how_metal()
  # @show @benchmark cost_of_playlist()
  # @show @benchmark revenue_per_track()
end

end
