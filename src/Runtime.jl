module Runtime

abstract Row

macro row(name, types)
  types = types.args
  :(begin
      immutable $(esc(name)) <: Row
        $([:($(symbol("f", i))::$(types[i])) for i in 1:length(types)]...)
      end
    end)
end

@generated function cmp_by_key{R1 <: Row, R2 <: Row}(x::R1, y::R2, xkey, ykey)
  xkey = xkey.parameters[1].parameters[1]
  ykey = ykey.parameters[1].parameters[1]
  @assert(length(xkey) == length(ykey))
  :(begin
      $([:(if !isequal(x.$(xkey[i]), y.$(ykey[i])); return isless(x.$(xkey[i]), y.$(ykey[i])) ? -1 : 1; end) for i in 1:length(xkey)]...)
      return 0
    end)
end

@generated function Base.isless{R <: Row}(x::R, y::R)
  key = [symbol("f", i) for i in 1:length(fieldnames(R))]
  last = pop!(key)
  :(begin
      $([:(if !isequal(x.$k, y.$k); return isless(x.$k, y.$k); end) for k in key]...)
      return isless(x.$last, y.$last)
    end)
end

@generated function construct{C,K}(constructor::Type{C}, key::Type{Val{K}}, value)
  constructor = constructor.parameters[1]
  fields = key.parameters[1].parameters[1]
  :(begin
      $constructor($([:(value.$field) for field in fields]...))
    end)
end

@generated function construct2{C,K1,K2}(constructor::Type{C}, key1::Type{Val{K1}}, key2::Type{Val{K2}}, value1, value2)
  constructor = constructor.parameters[1]
  fields1 = key1.parameters[1].parameters[1]
  fields2 = key2.parameters[1].parameters[1]
  :(begin
      $constructor(
        $([:(value1.$field) for field in fields1]...),
        $([:(value2.$field) for field in fields2]...),
        )
    end)
end

function dedup_sorted{X}(xs::Vector{X})
  ys = Vector{X}(0)
  last = xs[1]
  push!(ys, last)
  for x in xs
    if x != last
      push!(ys, x)
      last = x
    end
  end
  ys
end

function reshape{C,K}(xs, ys, ytype::Type{C}, ykey::Type{Val{K}})
  for x in xs
    push!(ys, construct(ytype, ykey, x))
  end
end

function project(xs, ytype, ykey)
  ys = Vector{ytype}(0)
  reshape(xs, ys, ytype, Val{ykey})
  sort!(ys, alg=QuickSort)
  dedup_sorted(ys)
end

function join_sorted_inner{X,Y,Z,XK,YK,ZK1,ZK2}(
  xs::Vector{X}, ys::Vector{Y}, ztype::Type{Z},
  xkey::Type{Val{XK}}, ykey::Type{Val{YK}}, zkey1::Type{Val{ZK1}}, zkey2::Type{Val{ZK2}}
  )
  zs = Vector{Z}(0)
  xi = 1
  yi = 1
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = cmp_by_key(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      xj = xi
      yj = yi
      while (xj <= length(xs)) && (cmp_by_key(x, xs[xj], xkey, xkey) == 0)
        xj += 1
      end
      while (yj <= length(ys)) && (cmp_by_key(y, ys[yj], ykey, ykey) == 0)
        yj += 1
      end
      for xk in xi:(xj-1)
        for yk in yi:(yj-1)
          push!(zs, construct2(Z, zkey1, zkey2, xs[xk], ys[yk]))
        end
      end
      xi = xj
      yi = yj
    end
  end
  zs
end

@inline function join_sorted(xs, ys, ztype, xkey, ykey, zkey1, zkey2)
  join_sorted_inner(xs, ys, ztype, Val{xkey}, Val{ykey}, Val{zkey1}, Val{zkey2})
end

function semijoin_sorted_inner{X,Y,XK,YK}(
  xs::Vector{X}, ys::Vector{Y},
  xkey::Type{Val{XK}}, ykey::Type{Val{YK}}
  )
  zs = Vector{X}(0)
  xi = 1
  yi = 1
  while (xi <= length(xs)) && (yi <= length(ys))
    x = xs[xi]
    y = ys[yi]
    c = cmp_by_key(x, y, xkey, ykey)
    if c == -1
      xi += 1
    elseif c == 1
      yi += 1
    else
      push!(zs, x)
      xi += 1
    end
  end
  zs
end

@inline function semijoin_sorted(xs, ys, xkey, ykey)
  semijoin_sorted_inner(xs, ys, Val{xkey}, Val{ykey})
end

function read_tsv(rowtype, filename) 
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

function chinook()
  (
    read_tsv(Artist, "/home/jamie/imp/data/Artist.csv"),
    read_tsv(Album, "/home/jamie/imp/data/Album.csv"),
    read_tsv(Track, "/home/jamie/imp/data/Track.csv"),
    read_tsv(PlaylistTrack, "/home/jamie/imp/data/PlaylistTrack.csv"),
    read_tsv(Playlist, "/home/jamie/imp/data/Playlist.csv"),
    )
end

@row(I1, [Int64, UTF8String]) # playlist_id playlist_name
@row(I2, [Int64, Int64]) # playlist_id track_id
@row(I3, [Int64, Int64]) # track_id playlist_id
@row(I4, [Int64, Int64]) # track_id album_id
@row(I5, [Int64, Int64]) # album_id track_id
@row(I6, [Int64, Int64]) # album_id artist_id
@row(I7, [Int64, Int64]) # artist_id album_id
@row(I8, [Int64, UTF8String]) # artist_id artist_name
@row(I9, [Int64, UTF8String]) # album_id artist_name
@row(I10, [Int64, UTF8String]) # track_id artist_name
@row(I11, [Int64, UTF8String]) # playlist_id artist_name
@row(I12, [UTF8String, UTF8String]) # playlist_name artist_name

function metal(data)
  i0 = data[5] # filter(row -> row.f2 == "Heavy Metal Classic", data[5])

  i1 = project(i0, I1, (1,2))
  i2 = project(data[4], I2, (1,2))
  i2s = semijoin_sorted(i2::Vector{I2}, i1::Vector{I1}, (1,), (1,))

  i3 = project(i2s, I3, (2,1))
  i4 = project(data[3], I4, (1,3))
  i4s = semijoin_sorted(i4, i3, (1,), (1,))

  i5 = project(i4s, I5, (2,1))
  i6 = project(data[2], I6, (1,3))
  i6s = semijoin_sorted(i6, i5, (1,), (1,))

  i7 = project(i6s, I7, (2,1))
  i8 = project(data[1], I8, (1,2))
  i9 = join_sorted(i7, i8, I9, (1,), (1,), (2,), (2,))

  i9s = project(i9, I9, (1,2))
  i10 = join_sorted(i5, i9s, I10, (1,), (1,), (2,), (2,))

  i10s = project(i10, I10, (1,2))
  i11 = join_sorted(i3, i10s, I11, (1,), (1,), (2,), (2,))

  i11s = project(i11, I11, (1,2))
  i12 = join_sorted(i1, i11s, I12, (1,), (1,), (2,), (2,))

  i12
end

@row(I0, [Int64])

function my_search(v, x)
  lo = 0
  hi = length(v)
  @inbounds while lo < hi-1
    m = (lo+hi)>>>1
    if v[m].f1 < x
      lo = m
    else
      hi = m
    end
  end
  return hi
end

function simple_sorted(i1,i2,i3,i4,i5)
  results = Int64[]
  for playlist in i5 
    if true # playlist.f1 == 13 
      playlist_id = playlist.f1
      ix0 = gallop(i4, playlist_id, 1)
      while (ix0 <= length(i4)) && (i4[ix0].f1 == playlist_id)
        track_id = i4[ix0].f2
        ix0 += 1
        ix1 = gallop(i3, track_id, 1)
        while (ix1 <= length(i3)) && (i3[ix1].f1 == track_id)
          album_id = i3[ix1].f2
          ix1 += 1
          ix2 = gallop(i2, album_id, 1)
          while (ix2 <= length(i2)) && (i2[ix2].f1 == album_id)
            artist_id = i2[ix2].f2
            ix2 += 1
            ix3 = gallop(i1, artist_id, 1)
            while (ix3 <= length(i1)) && (i1[ix3].f1 == artist_id)
              push!(results, i1[ix3].f1)
              ix3 += 1
            end
          end
        end
      end
    end
  end
  sort!(results, alg=QuickSort)
  dedup_sorted(results)
end

function simple_sorted_pre(data)
  i1 = project(data[1], I0, (1,))
  i2 = project(data[2], I2, (1,3))
  i3 = project(data[3], I2, (1,3))
  i4 = project(data[4], I2, (1,2))
  i5 = project(data[5], I0, (1,))
  (i1,i2,i3,i4,i5)
end

@row(I0, [Int64])

function simple_hashed(i1,i2,i3,i4,i5)
  results = Int64[]
  empty = Int64[]
  for playlist in i5 
    if true # playlist == 13 
      for track in get(i4, playlist, empty)
        for album in get(i3, track, empty)
          for artist in get(i2, album, empty)
            if artist in i1
              push!(results, artist)
            end
          end
        end
      end
    end
  end
  sort!(results, alg=QuickSort)
  dedup_sorted(results)
end

function simple_hashed_pre(data)
  i1 = [row.f1 for row in data[1]]
  i2 = Dict{Int64, Vector{Int64}}()
  for row in data[2]
    i2[row.f1] = Int64[]
  end
  for row in data[2]
    push!(i2[row.f1], row.f3)
  end
  i3 = Dict{Int64, Vector{Int64}}()
  for row in data[3]
    i3[row.f1] = Int64[]
  end
  for row in data[3]
    push!(i3[row.f1], row.f3)
  end
  i4 = Dict{Int64, Vector{Int64}}()
  for row in data[4]
    i4[row.f1] = Int64[]
  end
  for row in data[4]
    push!(i4[row.f1], row.f2)
  end
  i5 = Set{Int64}([row.f1 for row in data[5]])
  (i1,i2,i3,i4,i5)
end

function simple_forward(i1,i2,i3,i4,i5)
  tracks = Int64[]
  ix0 = 1
  for playlist in i5 
    if true # playlist.f1 == 13 
      playlist_id = playlist.f1
      ix0 = gallop(i4, playlist_id, ix0)
      while (ix0 <= length(i4)) && (i4[ix0].f1 == playlist_id)
        track_id = i4[ix0].f2
        ix0 += 1
        push!(tracks, track_id)
      end
    end
  end 
  sort!(tracks, alg=QuickSort)
  dedup_sorted(tracks)
  albums = Int64[]
  ix1 = 1
  for track_id in tracks 
    ix1 = gallop(i3, track_id, ix1)
    while (ix1 <= length(i3)) && (i3[ix1].f1 == track_id)
      album_id = i3[ix1].f2
      ix1 += 1
      push!(albums, album_id) 
    end 
  end
  sort!(albums, alg=QuickSort)
  dedup_sorted(albums)
  artists = Int64[]
  ix2 = 1
  for album_id in albums
    ix2 = gallop(i2, album_id, ix2)
    while (ix2 <= length(i2)) && (i2[ix2].f1 == album_id)
      artist_id = i2[ix2].f2
      ix2 += 1
      push!(artists, artist_id)
    end 
  end
  sort!(artists, alg=QuickSort)
  dedup_sorted(artists)
  results = Int64[]
  ix3 = 1
  for artist_id in artists
    ix3 = gallop(i1, artist_id, ix3)
    while (ix3 <= length(i1)) && (i1[ix3].f1 == artist_id)
      push!(results, i1[ix3].f1)
      ix3 += 1
    end
  end
  sort!(results, alg=QuickSort)
  dedup_sorted(results)
end

function simple_forward_pre(data)
  i1 = project(data[1], I0, (1,))
  i2 = project(data[2], I2, (1,3))
  i3 = project(data[3], I2, (1,3))
  i4 = project(data[4], I2, (1,2))
  i5 = project(data[5], I0, (1,))
  (i1,i2,i3,i4,i5)
end

function gallop(v, x, lo) 
  hi = length(v) + 1
  if (lo < hi) && (v[lo].f1 < x)
    step = 1
    while (lo + step < hi) && (v[lo + step].f1 < x)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && v[lo + step].f1 < x
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  lo 
end 

using Benchmark

function f()
  data = chinook()
  # @time benchmark(()->metal(data), "", 1000)
  @time benchmark(()->simple_sorted_pre(data), "", 1000)
  (i1,i2,i3,i4,i5) = simple_sorted_pre(data)
  @time benchmark(()->simple_sorted(i1,i2,i3,i4,i5), "", 1000)
  s = simple_sorted(i1,i2,i3,i4,i5)
  @time benchmark(()->simple_hashed_pre(data), "", 1000)
  (i1,i2,i3,i4,i5) = simple_hashed_pre(data)
  @time benchmark(()->simple_hashed(i1,i2,i3,i4,i5), "", 1000)
  h = simple_hashed(i1,i2,i3,i4,i5)
  @time benchmark(()->simple_forward_pre(data), "", 1000)
  (i1,i2,i3,i4,i5) = simple_forward_pre(data)
  @time benchmark(()->simple_forward(i1,i2,i3,i4,i5), "", 1000)
  f = simple_forward(i1,i2,i3,i4,i5)
  println(s)
  println(h)
  println(f)
  assert(s == h)
  assert(h == f)
end

immutable Row2{A,B} 
  a::A 
  b::B
end 

@inline function Base.isless{A,B}(x::Row2{A,B}, y::Row2{A,B})
  if !isequal(x.a, y.a); return Base.isless(x.a, y.a); end
  return Base.isless(x.b, y.b)
end
# 
# abstract Columns{T} <: AbstractVector{T}
# 
# type Columns1{A} <: Columns{Tuple{A}}
#   as::Vector{A}
# end
# 
# function Base.length{A}(c1::Columns1{A}) 
#   length(c1.as)
# end
# 
# function Base.getindex{A}(c1::Columns1{A}, ix)
#   (c1.as[ix],)
# end
# 
# function Base.setindex!{A}(c1::Columns1{A}, val::Tuple{A}, ix)
#   c1.as[ix] = val[1]
# end
# 
# function Base.size{A}(c1::Columns1{A})
#   (length(c1),)
# end
  
type Columns2{A,B} # <: Columns{Row2{A,B}}
  as::Vector{A}
  bs::Vector{B}
end

function Base.length{A,B}(c2::Columns2{A,B}) 
  length(c2.as)
end

# @inline function Base.getindex{A,B}(c2::Columns2{A,B}, ix)
#   Row2(c2.as[ix], c2.bs[ix])
# end
# 
# @inline function Base.setindex!{A,B}(c2::Columns2{A,B}, val::Row2{A,B}, ix)
#   c2.as[ix] = val.a
#   c2.bs[ix] = val.b
# end

function Base.size{A,B}(c2::Columns2{A,B})
  (length(c2),)
end

@inline function lt(o, v, i, j) 
  #@inbounds return (v.as[i] < v.as[j]) || (v.as[i] == v.as[j] && v.bs[i] < v.bs[j])
  if !isequal(v.as[i], v.as[j]); return isless(v.as[i], v.as[j]); end 
  return isless(v.bs[i], v.bs[j])
end

@inline function swap(v, i, j)
  @inbounds begin
    v.as[i], v.as[j] = v.as[j], v.as[i]
    v.bs[i], v.bs[j] = v.bs[j], v.bs[i]
  end
end

@inline function swap(v, i, j, k)
  @inbounds begin
    v.as[i], v.as[j], v.as[k] = v.as[k], v.as[i], v.as[j]
    v.bs[i], v.bs[j], v.bs[k] = v.bs[k], v.bs[i], v.bs[j]
  end
end

@inline function Base.sort!{A,B}(v::Columns2{A,B}, lo::Int, hi::Int, ::Base.Sort.InsertionSortAlg, o::Base.Order.Ordering)
    @inbounds for i = lo+1:hi
        j = i
        while j > lo
            if lt(o, v, j, j-1)
                swap(v, j-1, j)
                j -= 1
                continue
            end
            break
        end
    end
    return v
end

@inline function Base.Sort.selectpivot!{A,B}(v::Columns2{A,B}, lo::Int, hi::Int, o::Base.Order.Ordering)
    @inbounds begin
        mi = (lo+hi)>>>1

        # sort the values in v[lo], v[mi], v[hi]

        if lt(o, v, mi, lo)
            swap(v, lo, mi)
        end
        if lt(o, v, hi, mi)
            if lt(o, v, hi, lo)
                swap(v, lo, mi, hi)
            else
                swap(v, mi, hi)
            end
        end

        # move v[mi] to v[lo] and use it as the pivot
        swap(v, lo, mi)
    end

    # return the pivot
    return lo
end

function Base.Sort.partition!{A,B}(v::Columns2{A,B}, lo::Int, hi::Int, o::Base.Order.Ordering)
    pivot = Base.Sort.selectpivot!(v, lo, hi, o)
    # pivot == v[lo], v[hi] > pivot
    i, j = lo, hi
    @inbounds while true
        i += 1; j -= 1
        while lt(o, v, i, pivot); i += 1; end;
        while lt(o, v, pivot, j); j -= 1; end;
        i >= j && break
        swap(v, i, j)
    end
    a = v.as[j]
    b = v.bs[j]
    v.as[j] = v.as[pivot]
    v.bs[j] = v.bs[pivot]
    v.as[lo] = a
    v.bs[lo] = b
    # v.as[j] = v.as[]
    # v[j], v[lo] = v[pivot], v[j]

    # v[j] == pivot
    # v[k] >= pivot for k > j
    # v[i] <= pivot for i < j
    return j
end

function Base.Sort.sort!{A,B}(v::Columns2{A,B}, lo::Int, hi::Int, a::Base.Sort.QuickSortAlg, o::Base.Order.Ordering)
    @inbounds while lo < hi
        hi-lo <= Base.Sort.SMALL_THRESHOLD && return sort!(v, lo, hi, Base.Sort.SMALL_ALGORITHM, o)
        j = Base.Sort.partition!(v, lo, hi, o)
        if j-lo < hi-j
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
            lo < (j-1) && sort!(v, lo, j-1, a, o)
            lo = j+1
        else
            j+1 < hi && sort!(v, j+1, hi, a, o)
            hi = j-1
        end
    end
    return v
end

n = Int64(1E6)
make_ids() = rand(1:n, n)

ids = make_ids()
@time sort!(ids, alg=QuickSort)

c2 = Columns2(make_ids(), [1 for _ in ids])
@time sort!(c2, 1, length(c2), QuickSort, Base.Order.Forward)
println(c2.as[1:100])
as = copy(c2.as)
sort!(as)
assert(c2.as == as)

type Rows2{A,B} <: AbstractVector{Row2{A,B}}
  as::Vector{A}
  bs::Vector{B}
end

function Base.length{A,B}(rs::Rows2{A,B})
  length(rs.as)
end

function Base.size{A,B}(rs::Rows2{A,B})
  (length(rs.as),)
end

function Base.getindex{A,B}(rs::Rows2{A,B}, i)
  Row2(rs.as[i], rs.bs[i])
end

function Base.setindex!{A,B}(rs::Rows2{A,B}, r::Row2{A,B}, i)
  rs.as[i] = r.a
  rs.bs[i] = r.b
end

rs = Rows2(make_ids(), ["a" for _ in make_ids()])
@time sort!(rs, alg=QuickSort)

c1 = Row2{Int64, Int64}[Row2(i,1) for i in make_ids()]
@time sort!(c1, alg=QuickSort)



# @code_warntype sort!(c2, 1, length(c2), QuickSort, Base.Order.Forward)
# @code_warntype Base.Sort.partition!(c2, 1, length(c2), Base.Order.Forward)
# @code_warntype sort!(c2, 1, length(c2), InsertionSort, Base.Order.Forward)
# @code_native Base.Sort.partition!(c2, 1, length(c2), Base.Order.Forward)

1

end
