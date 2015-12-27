key_cmp{X,Y}(x::X, y::Y, xkey::Vector{Int}, ykey::Vector{Int}) = begin
  for i in 1:length(xkey)
    c = cmp(x[xkey[i]],y[ykey[i]])
    if c == -1
      return -1
    elseif c == 1
      return 1
    end
  end
  return 0
end

join_sorted{X,Y}(xs::Vector{X}, ys::Vector{Y}, xkey::Vector{Int}, ykey::Vector{Int}) = begin
  xi = 1
  yi = 1
  zs = Vector{Tuple{X,Y}}(0)
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = key_cmp(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      push!(zs, (x, y))
      xi += 1
      yi += 1
    end
  end
  zs
end

semijoin_sorted{X,Y}(xs::Vector{X}, ys::Vector{Y}, xkey::Vector{Int}, ykey::Vector{Int}) = begin
  xi = 1
  yi = 1
  zs = Vector{Tuple{X}}(0)
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = key_cmp(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      push!(zs, x)
      xi += 1
      yi += 1
    end
  end
  zs
end

immutable Row
  a::Int64
  b::Int64
end

Base.isless(x::Row, y::Row) =
  isless(x.a, y.a) || (isequal(x.a, y.a) && isless(x.b, y.b))

macro construct(ytype, x, key...)
  :( ($ytype)($([:($x.$k) for k in key]...)) )
end

macro project(xs, ytype, key...)
  x = gensym("x")
  :(begin
      xs = $(esc(xs))
      ys = Vector{$ytype}(0)
      for $x in xs
        y = @construct($ytype, $x, $(key...))
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

f() = begin
  xs = Row[Row(a,b) for (a,b) in zip(ids(), ids())]
  @time @project(xs, Row, b, a)
end

f()

using Benchmark
ids() = ids(1000000)
ids(n) = rand(1:n, n)
test_join() = begin
  xs = collect(zip(ids(), ids(), ids()))
  ys = collect(zip(ids(), ids(), ids()))
  sort!(xs)
  sort!(ys)
#   benchmark(()->join_sorted(xs, ys, [1], [1]), "", 100)
  @time join_sorted(xs, ys, [1], [1])
end

read_tsv(filename, types) = begin
  println(filename)
  raw = readdlm(filename, '\t', UTF8String, header=true, quotes=false, comments=false)[1]
  results = Vector{Tuple{types...}}(0)
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
