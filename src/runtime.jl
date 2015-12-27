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

read_tsv(filename, rowtype, fieldtypes) = begin
  println(filename)
  raw = readdlm(filename, '\t', UTF8String, header=true, quotes=false, comments=false)[1]
  results = Vector{rowtype}(0)
  for i in 1:size(raw,1)
    row = Vector{Any}(vec(raw[i,:]))
    for j in 1:length(types)
      if issubtype(types[j], Number)
        row[j] = parse(types[j], row[j])
      end
    end
    push!(results, tuple(row...))
  end
  results
end

chinook() = begin
  Any[
    read_tsv("code/imp/data/Artist.csv", [Int64, UTF8String]),
    read_tsv("code/imp/data/Album.csv", [Int64, UTF8String, Int64]),
    read_tsv("code/imp/data/Track.csv", [Int64, UTF8String, Int64, Int64, Int64, UTF8String, Float64, Float64, Float64]),
    read_tsv("code/imp/data/PlaylistTrack.csv", [Int64, Int64]),
    read_tsv("code/imp/data/Playlist.csv", [Int64, UTF8String]),
    ]
end

metal(data) = begin
    data[5] = filter(data[5], row -> row[2] == "Heavy Metal Classic")
    data[5] = sort_by_key
