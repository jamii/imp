module Runtime

macro construct(ytype, x, key)
  key = eval(key)
  :( ($ytype)($([:($x.$k) for k in key]...)) )
end

macro project(xs, xtype, ytype, key)
  :(begin
      xs::Vector{$xtype} = $(esc(xs))
      ys = Vector{$ytype}(0)
      for x in xs
        y = @construct($ytype, x, $key)
        push!(ys, y)
      end
      sort!(ys, alg=QuickSort)
      zs = Vector{$ytype}(0)
      last = ys[1]
      push!(zs, last)
      for y in ys
        if y != last
          push!(zs, y)
          last = y
        end
      end
      zs
    end)
end

macro cmp_by_key(x, y, xkey, ykey)
  xkey = eval(xkey)
  ykey = eval(ykey)
  @assert(length(xkey) == length(ykey))
  c = gensym("c")
  :(begin
      x = $(esc(x))
      y = $(esc(y))
      $c = cmp(x.$(xkey[1]), y.$(ykey[1]))
      $([:(if $c == 0; $c = cmp(x.$(xkey[i]), y.$(ykey[i])) end) for i in 2:length(xkey)]...)
      $c
    end)
end

macro construct2(ztype, x, y, xkey, ykey)
  xkey = eval(xkey)
  ykey = eval(ykey)
  :( ($ztype)($([[:($x.$k) for k in xkey] ; [:($y.$k) for k in ykey]]...)) )
end

macro join_sorted(xs, ys, xtype, ytype, xkey, ykey, ztype, zkey1, zkey2)
  :(begin
      xs::Vector{$xtype} = $(esc(xs))
      ys::Vector{$ytype} = $(esc(ys))
      xi = 1
      yi = 1
      zs = Vector{$ztype}(0)
      while (xi <= length(xs)) && (yi <= length(ys))
        x = xs[xi]
        y = ys[yi]
        c = @cmp_by_key(x, y, $xkey, $ykey)
        if c == -1
          xi += 1
        elseif c == 1
          yi += 1
        else
          xj = xi
          yj = yi
          while (xj <= length(xs)) && @cmp_by_key(x, xs[xj], $xkey, $xkey) == 0
            xj += 1
          end
          while (yj <= length(ys)) && @cmp_by_key(y, ys[yj], $ykey, $ykey) == 0
            yj += 1
          end
          for xk in xi:(xj-1)
            for yk in yi:(yj-1)
              push!(zs, @construct2($ztype, xs[xk], ys[yk], $zkey1, $zkey2))
            end
          end
          xi = xj
          yi = yj
        end
      end
      zs
    end)
end

macro semijoin_sorted(xs, ys, xtype, ytype, xkey, ykey, ztype, zkey)
  :(begin
      xs::Vector{$xtype} = $(esc(xs))
      ys::Vector{$ytype} = $(esc(ys))
      xi = 1
      yi = 1
      zs = Vector{$ztype}(0)
      while (xi <= length(xs)) && (yi <= length(ys))
        x = xs[xi]
        y = ys[yi]
        c = @cmp_by_key(x, y, $xkey, $ykey)
        if c == -1
          xi += 1
        elseif c == 1
          yi += 1
        else
          push!(zs, @construct($ztype, x, $zkey))
          xi += 1
        end
      end
      zs
    end)
end

macro row(name, types)
  types = eval(types)
  :(begin
      immutable $name
        $([:($(symbol("f", i))::$(types[i])) for i in 1:length(types)]...)
      end
      Base.isless(x::$name, y::$name) = begin
        c = @cmp_by_key(x, y, $([symbol("f", i) for i in 1:length(types)]), $([symbol("f", i) for i in 1:length(types)]))
        c == -1
      end
    end)
end

read_tsv(rowtype, filename) = begin
  println(filename)
  fieldtypes = [fieldtype(rowtype, fieldname) for fieldname in fieldnames(rowtype)]
  raw = readdlm(filename, '\t', UTF8String, header=true, quotes=false, comments=false)[1]
  results = Vector{rowtype}(0)
  for i in 1:size(raw,1)
    row = Vector{Any}(vec(raw[i,:]))
    for j in 1:length(fieldtypes)
      if issubtype(fieldtypes[j], Number)
        row[j] = parse(fieldtypes[j], row[j])
      end
    end
    push!(results, rowtype(row...))
  end
  results
end

@row(Artist, [Int64, UTF8String])
@row(Album, [Int64, UTF8String, Int64])
@row(Track, [Int64, UTF8String, Int64, Int64, Int64, UTF8String, Float64, Float64, Float64])
@row(PlaylistTrack, [Int64, Int64])
@row(Playlist, [Int64, UTF8String])

chinook() = begin
  Any[
    read_tsv(Artist, "code/imp/data/Artist.csv"),
    read_tsv(Album, "code/imp/data/Album.csv"),
    read_tsv(Track, "code/imp/data/Track.csv"),
    read_tsv(PlaylistTrack, "code/imp/data/PlaylistTrack.csv"),
    read_tsv(Playlist, "code/imp/data/Playlist.csv"),
    ]
end

@row(I1, [Int64, UTF8String]) # playlist_id playlist_name
@row(I2, [Int64, Int64]) # playlist_id track_id
@row(I3, [Int64, Int64]) # track_id playlist_id
@row(I4, [Int64, Int64]) # track_id album_id
@row(I5, [Int64, Int64]) # album_id track_id
@row(I6, [Int64, Int64]) # album_id artist_id
@row(I7, [Int64, Int64]) # artist_id album_id
@row(I8, [Int64, UTF8String]) # album_id artist_id

metal(data) = begin
  @time data[5] = filter(row -> row.f2 == "Heavy Metal Classic", data[5])

  @time i1 = @project(data[5], Playlist, I1, [:f1 :f2])
  @time i2 = @project(data[4], PlaylistTrack, I2, [:f1 :f2])
  @time i2s = @semijoin_sorted(i2, i1, I2, I1, [:f1], [:f1], I2, [:f1 :f2])

  @time i3 = @project(i2s, I2, I3, [:f2 :f1])
  @time i4 = @project(data[3], Track, I4, [:f1 :f3])
  @time i4s = @semijoin_sorted(i4, i3, I4, I3, [:f1], [:f1], I4, [:f1 :f2])

  @time i5 = @project(i4s, I4, I5, [:f2 :f1])
  @time i6 = @project(data[2], Album, I6, [:f1 :f3])
  @time i6s = @semijoin_sorted(i6, i5, I6, I5, [:f1], [:f1], I6, [:f1 :f2])

  @time i7 = @project(i6s, I6, I7, [:f2 :f1])
  @time i8 = @project(data[1], Artist, I8, [:f1 :f2])
  @time i8s = @semijoin_sorted(i8, i7, I8, I7, [:f1], [:f1], I8, [:f1 :f2])
end

go() = begin
  c = chinook()
  @time metal(c)
  c
end

go()

end
