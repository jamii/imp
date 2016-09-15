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
  Relation(columns, length(columns))
end

const album = read_chinook("data/Album.csv", [Int64, String, Int64])
const artist = read_chinook("data/Artist.csv", [Int64, String])
const track = read_chinook("data/Track.csv", [Int64, String, Int64, Int64, Int64, String, Int64, Int64, Float64])
const playlist_track = read_chinook("data/PlaylistTrack.csv", [Int64, Int64])
const playlist = read_chinook("data/Playlist.csv", [Int64, String])

function who_is_metal()
  @query begin
    playlist(playlist, "Heavy Metal Classic")
    playlist_track(playlist, track)
    track(track, _, album)
    album(album, _, artist)
    artist(artist, artist_name)
    return (artist_name,)
  end
end

function cost_of_playlist()
  @query begin
    playlist(p, pn)
    tracks = @query begin 
      playlist_track($p, t)
      track(t, _, _, _, _, _, _, _, price)
      return (t, price)
    end
    total = sum(tracks.columns[2])
    return (pn, total)
  end
end

function revenue_per_track()
  @query begin
    track(t, tn, _, _, _, _, _, _, price)
    plays = @query begin 
      playlist_track(p, $t)
      return (p::Int64,)
    end
    total = price * length(plays)
    return (tn, total)
  end
end

function test()
  @test who_is_metal().columns == (
  String["AC/DC","Accept","Black Sabbath","Iron Maiden","Metallica","Motörhead","Mötley Crüe","Ozzy Osbourne","Scorpions"],
  )
  @test cost_of_playlist().columns == (
  String["90’s Music","Audiobooks","Brazilian Music","Classical","Classical 101 - Deep Cuts","Classical 101 - Next Steps","Classical 101 - The Basics","Grunge","Heavy Metal Classic","Movies","Music","Music Videos","On-The-Go 1","TV Shows"],
  Float64[2.490879e6,0.0,46631.0,258700.0,87275.0,86050.0,85375.0,31832.0,34864.0,0.0,5.487052e6,3402.0,597.0,650204.0]
  )
  @test revenue_per_track().columns[1][1:10] == String["\"40\"","\"?\"","\"Eine Kleine Nachtmusik\" Serenade In G, K. 525: I. Allegro","#1 Zero","#9 Dream","'Round Midnight","(Anesthesia) Pulling Teeth","(Da Le) Yaleo","(I Can't Help) Falling In Love With You","(Oh) Pretty Woman"]
  @test revenue_per_track().columns[2][1:10] == Float64[1.98,3.98,3.96,1.98,1.98,1.98,1.98,2.9699999999999998,2.9699999999999998,1.98]
end

function bench()
  @show @benchmark who_is_metal()
  @show @benchmark cost_of_playlist()
  @show @benchmark revenue_per_track()
end

end
