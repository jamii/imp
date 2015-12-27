module Runtime

macro construct(ytype, x, key)
  key = eval(key)
  :( ($ytype)($([:($x.$k) for k in key]...)) )
end

macro project(xs, ytype, key)
  :(begin
      xs = $(esc(xs))
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

macro join_sorted(xs, ys, xkey, ykey, ztype, zkey1, zkey2)
  :(begin
      xs = $(esc(xs))
      ys = $(esc(ys))
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
          push!(zs, @construct2($ztype, x, y, $zkey1, $zkey2))
          xi += 1
          yi += 1
        end
      end
      zs
    end)
end

macro semijoin_sorted(xs, ys, xkey, ykey, ztype, zkey)
  :(begin
      xs = $(esc(xs))
      ys = $(esc(ys))
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
          yi += 1
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

@row(I1, [Int64, UTF8String])
@row(I2, [Int64, Int64])

metal(data) = begin
  data[5] = filter(row -> row.f2 == "Heavy Metal Classic", data[5])

  data[5] = @project(data[5], I1, [:f1 :f2])
  data[4] = @project(data[4], I2, [:f2 :f1])
end

begin
  c = chinook()
  metal(c)
  c
end

end

reload("Runtime")
