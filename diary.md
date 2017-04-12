### 2016 Jul 27

I have a plan and it starts with some sorted arrays.

Ideally I would just throw some tuples or structs into an array and sort it. Unfortunately, Julia still has this restriction on structs that contain pointers. Everything is happy as long as I stick to PODs but as soon as I want to have, say, a string column, I suddenly end up with an array of pointers to heap-allocated rows. Which is not what I want at all. 

``` julia 
r2 = [(id::Int64, id::Int64) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.056419 seconds (5 allocations: 240 bytes)

r2 = [(id::Int64, string(id)::ASCIIString) for id in ids]
@time sort!(r2, alg=QuickSort)
# 2.340892 seconds (34.94 M allocations: 533.120 MB)
# heap-allocated *and* pass-by-value! tuples are weird!

r2 = [Row(id::Int64, id::Int64) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.058970 seconds (5 allocations: 240 bytes)

r2 = [Row(id::Int64, string(id)::ASCIIString) for id in ids]
@time sort!(r2, alg=QuickSort)
# 0.124810 seconds (5 allocations: 240 bytes)
```

We can get round this by flipping the layout into columns, but we still need to sort it. Julia's standard sort function only requires length, getindex and setindex:

``` julia 
type Columns2{A,B} <: Columns{Row2{A,B}}
  as::Vector{A}
  bs::Vector{B}
end

function Base.length{A,B}(c2::Columns2{A,B}) 
  length(c2.as)
end

@inline function Base.getindex{A,B}(c2::Columns2{A,B}, ix)
  Row2(c2.as[ix], c2.bs[ix])
end

@inline function Base.setindex!{A,B}(c2::Columns2{A,B}, val::Row2{A,B}, ix)
  c2.as[ix] = val.a
  c2.bs[ix] = val.b
end
```

But these still have to return something row-like which leaves us with exactly the same problem:

``` julia 
c2 = Columns2([id::Int64 for id in ids], [id::Int64 for id in ids])
@time sort!(c2, alg=QuickSort)
# 0.056417 seconds (5 allocations: 240 bytes)

c2 = Columns2([id::Int64 for id in ids], [string(id)::ASCIIString for id in ids])
@time sort!(c2, alg=QuickSort)
# 0.542212 seconds (19.06 M allocations: 582.780 MB, 46.45% gc time)
```

I would enjoy Julia a lot more if this wasn't a thing.

So, let's just brute-force a workaround. I'll copy the sorting code from the base library and generate different versions of it for every number of columns, using multiple variables to hold the values instead of tuples or structs.

``` julia 
function define_columns(n)
  cs = [symbol("c", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  tmps = [symbol("tmp", c) for c in 1:n]
  
  :(begin
  
  @inline function lt($(cs...), i, j) 
    @inbounds begin 
      $([:(if !isequal($(cs[c])[i], $(cs[c])[j]); return isless($(cs[c])[i], $(cs[c])[j]); end) for c in 1:(n-1)]...)
      return isless($(cs[n])[i], $(cs[n])[j])
    end
  end
  
  @inline function lt2($(cs...), $(tmps...), j) 
    @inbounds begin 
      $([:(if !isequal($(tmps[c]), $(cs[c])[j]); return isless($(tmps[c]), $(cs[c])[j]); end) for c in 1:(n-1)]...)
      return isless($(tmps[n]), $(cs[n])[j])
    end
  end
  
  @inline function swap2($(cs...), i, j)
    @inbounds begin
      $([:(begin
      $(tmps[c]) = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
  end
  end

  @inline function swap3($(cs...), i, j, k)
    @inbounds begin
      $([:(begin
      $(tmps[c]) = $(cs[c])[k]
      $(cs[c])[k] = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end) for c in 1:n]...)
  end
  end
  
  # sorting cribbed from Base.Sort
  
  function insertion_sort!($(cs...), lo::Int, hi::Int)
      @inbounds for i = lo+1:hi
        j = i
        $([:($(tmps[c]) = $(cs[c])[i]) for c in 1:n]...)
        while j > lo
            if lt2($(cs...), $(tmps...), j-1)
              $([:($(cs[c])[j] = $(cs[c])[j-1]) for c in 1:n]...)
              j -= 1
              continue
            end
            break
        end
        $([:($(cs[c])[j] = $(tmps[c])) for c in 1:n]...)
    end
  end

  @inline function select_pivot!($(cs...), lo::Int, hi::Int)
      @inbounds begin
          mi = (lo+hi)>>>1
          if lt($(cs...), mi, lo)
              swap2($(cs...), lo, mi)
          end
          if lt($(cs...), hi, mi)
              if lt($(cs...), hi, lo)
                  swap3($(cs...), lo, mi, hi)
              else
                  swap2($(cs...), mi, hi)
              end
          end
          swap2($(cs...), lo, mi)
      end
      return lo
  end

  function partition!($(cs...), lo::Int, hi::Int)
      pivot = select_pivot!($(cs...), lo, hi)
      i, j = lo, hi
      @inbounds while true
          i += 1; j -= 1
          while lt($(cs...), i, pivot); i += 1; end;
          while lt($(cs...), pivot, j); j -= 1; end;
          i >= j && break
          swap2($(cs...), i, j)
      end
      swap2($(cs...), pivot, j)
      return j
  end

  function quicksort!($(cs...), lo::Int, hi::Int)
      @inbounds while lo < hi
          if hi-lo <= 20 
            insertion_sort!($(cs...), lo, hi)
            return 
          end
          j = partition!($(cs...), lo, hi)
          if j-lo < hi-j
              lo < (j-1) && quicksort!($(cs...), lo, j-1)
              lo = j+1
          else
              j+1 < hi && quicksort!($(cs...), j+1, hi)
              hi = j-1
          end
      end
      return
  end
  
  function quicksort!{$(ts...)}(cs::Tuple{$(ts...)})
    quicksort!($([:(cs[$c]) for c in 1:n]...), 1, length(cs[1]))
    return cs
  end
  end)
end

for i in 1:10
  eval(define_columns(i))
end
```

It's not pretty. But...

``` julia 
c2 = ([id::Int64 for id in ids], [id::Int64 for id in ids])
@time quicksort!(c2)
# 0.017385 seconds (4 allocations: 160 bytes)

c2 = ([id::Int64 for id in ids], [string(id)::ASCIIString for id in ids])
@time quicksort!(c2)
# 0.053001 seconds (4 allocations: 160 bytes)
```

Onwards.

### 2016 Jul 28

I kinda thought that Julia specialized on closures, but this turns out not to be true in the current release. So I upgraded to v0.5-rc0 and then spent most of the day persuading Juno to cooperate. I lost a lot of time before realizing that the Ubuntu 'nightly' PPA hasn't been updated in two months. After switching to the generic linux build and patching Juno in a few places it mostly works now, apart from a weird issue where displaying results inline in Atom sometimes leaves Julia spinning for minutes. 

But with that out of the way, we can write a really cute version of leapfrog triejoin:

``` julia 
# gallop cribbed from http://www.frankmcsherry.org/dataflow/relational/join/2015/04/11/genericjoin.html
function gallop{T}(column::Vector{T}, value::T, lo::Int64, hi::Int64, cmp) 
  if (lo < hi) && cmp(column[lo], value)
    step = 1
    while (lo + step < hi) && cmp(column[lo + step], value)
      lo = lo + step 
      step = step << 1
    end
    
    step = step >> 1
    while step > 0
      if (lo + step < hi) && cmp(column[lo + step], value)
        lo = lo + step 
      end
      step = step >> 1
    end
    
    lo += 1
  end
  lo 
end 

@inline function intersect{T,N}(handler, cols::NTuple{N, Vector{T}}, los::Vector{Int64}, his::Vector{Int64})
  # assume los/his are valid 
  # los inclusive, his exclusive
  n = length(cols)
  local value::T
  value = cols[n][los[n]]
  inited = false
  while true 
    for c in 1:n 
      if inited && (cols[c][los[c]] == value)
        matching_his = [gallop(cols[c], value, los[c], his[c], <=) for c in 1:n]
        handler(value, los, matching_his)
        los[c] = matching_his[c]
        # TODO can we set los = matching_his without breaking the stop condition?
      else 
        los[c] = gallop(cols[c], value, los[c], his[c], <)
      end
      if los[c] >= his[c]
        return 
      else 
        value = cols[c][los[c]]
      end
    end
    inited = true
  end
end
```

It's really unoptimised at the moment - I need to reuse allocations, remove bounds/null checks, unroll loops etc. But it seems to work:

``` julia 
function f() 
  edges_x = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
  edges_y = [[1, 2, 3, 3, 4], [2, 3, 1, 4, 2]]
  edges_z = [[1, 2, 2, 3, 4], [3, 1, 4, 2, 3]]
  intersect((edges_x[1], edges_z[1]), [1,1], [6,6]) do x, x_los, x_his
    intersect((edges_x[2], edges_y[1]), [x_los[1],1], [x_his[1],6]) do y, y_los, y_his
      intersect((edges_y[2], edges_z[2]), [y_los[2], x_los[2]], [y_his[2], x_his[2]]) do z, z_los, z_his
        println(x,y,z)
      end
    end
  end
end
```

It needed a bit of help typing `value` for some reason, and it insists on boxing it, but the code for `f` looks good otherwise. No generic calls and all the intersections are inlined.

### 2016 Jul 29

Ooops, the anonymous functions aren't inlined. Can fix that pretty easily:

``` julia 
@time intersect((edges_x[1], edges_z[1]), (1,1), (n,n), @inline function (x, x_los, x_his)
  intersect((edges_x[2], edges_y[1]), (x_los[1],1), (x_his[1],n), @inline function (y, y_los, y_his)
    intersect((edges_y[2], edges_z[2]), (y_los[2], x_los[2]), (y_his[2], x_his[2]), @inline function (z, z_los, z_his)
      println(x,y,z)
    end)
  end)
end)
```

I had to change the syntax because `@inline` is fussy about about what it accepts. I guess it wasn't intended for use with anonymous functions, because they were specialized on there was no opportunity to inline them anyway.

I cleaned up most of the obvious allocations by changing arrays to tuples, and unpacking them in the function body. That requires unrolling the inner loops too, which is probably not harmful. 

``` julia 
@generated function intersect{T,N}(cols::NTuple{N, Vector{T}}, los::NTuple{N, Int64}, his::NTuple{N, Int64}, handler)
  # assume los/his are valid 
  # los inclusive, his exclusive
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      local value::$T
      @nextract $N col cols
      @nextract $N lo los 
      @nextract $N hi his 
      value = col_1[lo_1]
      inited = false
      while true 
        @nexprs $N c->
        begin
          if inited && (col_c[lo_c] == value)
            @nexprs $N c2-> matching_hi_c2 = gallop(col_c2, value, lo_c2, hi_c2, <=)
            handler(value, (@ntuple $N lo), (@ntuple $N matching_hi))
            lo_c = matching_hi_c
            # TODO can we set los = matching_his without breaking the stop condition?
          else 
            lo_c = gallop(col_c, value, lo_c, hi_c, <)
          end
          if lo_c >= hi_c
            return 
          else 
            value = col_c[lo_c]
          end
          inited = true
        end
      end
    end
  end
end
```

It's nice that the facilities exist to do this kind of code rewriting, but I wouldn't have to do it in the first place if I could just mutate some stack-allocated tuple-like thing. Like a grownup.

Annoyingly, there is still a lot of allocation going on. Looking at the generated code it seems that, while all the anonymous functions have been inlined, the closures are still being created. And heap-allocated :(
  
It also looks like any values that are closed over become boxed, presumably because Julia can't guarantee that the closure doesn't escape the lifetime of the current stackframe. But the box doesn't get a type and that messed up downsteam inference - note the return type of `f` is `ANY` rather than `Int64`.

``` julia 
function f(xs)
  const t = 0
  foreach(xs) do x
    t += x
  end
  t
end
```

``` julia
Variables:
  #self#::Relation.#f
  xs::Array{Int64,1}
  t::CORE.BOX
  #43::Relation.##43#44

Body:
  begin 
      t::CORE.BOX = $(Expr(:new, :(Core.Box)))
      (Core.setfield!)(t::CORE.BOX,:contents,0)::Int64 # line 213:
      #43::Relation.##43#44 = $(Expr(:new, :(Relation.##43#44), :(t)))
      SSAValue(0) = #43::Relation.##43#44
      $(Expr(:invoke, LambdaInfo for foreach(::Relation.##43#44, ::Array{Int64,1}), :(Relation.foreach), SSAValue(0), :(xs))) # line 216:
      return (Core.getfield)(t::CORE.BOX,:contents)::ANY
  end::ANY
```

It looks like Julia's closures just aren't there yet.

### 2016 Jul 30

I managed a macro-y version that does the trick, producing zero allocations in the main body. The nice `@nexprs` macro I was using before doesn't interact well with the macro hygienisation so I have to do stuff by hand, with much additional syntax.

``` julia 
function unpack(expr)
  assert(expr.head == :tuple)
  for value in expr.args
    assert(typeof(value) == Symbol)
  end
  expr.args
end

macro intersect(cols, los, ats, his, next_los, next_his, handler)
  cols = unpack(cols)
  los = unpack(los)
  ats = unpack(ats)
  his = unpack(his)
  next_los = unpack(next_los)
  next_his = unpack(next_his)
  n = length(cols)
  quote
    # assume los/his are valid 
    # los inclusive, his exclusive
    @inbounds begin 
      $([
      quote
        $(esc(ats[c])) = $(esc(los[c]))
      end
      for c in 1:n]...)
      value = $(esc(cols[n]))[$(esc(ats[n]))]
      fixed = 1
      finished = false
      while !finished
        $([
        quote
          if fixed == $n 
            $([
            quote
              $(esc(next_los[c2])) = $(esc(ats[c2]))
              $(esc(next_his[c2])) = gallop($(esc(cols[c2])), value, $(esc(ats[c2])), $(esc(his[c2])), <=)
              $(esc(ats[c2])) = $(esc(next_his[c2]))
            end
            for c2 in 1:n]...)
            $handler # TODO huge code duplication
          else 
            $(esc(ats[c])) = gallop($(esc(cols[c])), value, $(esc(ats[c])), $(esc(his[c])), <)
          end
          if $(esc(ats[c])) >= $(esc(his[c]))
            finished = true
          else 
            next_value = $(esc(cols[c]))[$(esc(ats[c]))]
            fixed = (value == next_value) ? fixed+1 : 1
            value = next_value
          end
          inited = true
        end
        for c in 1:n]...)
      end
    end
  end
end
```

This is fast but awful to look at, so I played around with closures some more. I discovered that boxing of closed-over variables only happens if a stack-allocated thing is mutated. Heap-allocated things propagate their types just fine. (I'm sure I had a case where a stack-allocated thing got boxed without being mutated. Not sure if I imagined it or if the nest of closures was confusing the mutation analysis.)

``` julia 
function f(xs)
  t = [0]
  foreach(xs) do x
    t[1] += x
  end
  t[1]
end
```

``` julia 
Variables:
  #self#::Relation.#f
  xs::Array{Int64,1}
  t::Array{Int64,1}
  #267::Relation.##267#268{Array{Int64,1}}

Body:
  begin 
      t::Array{Int64,1} = $(Expr(:invoke, LambdaInfo for vect(::Int64, ::Vararg{Int64,N}), :(Base.vect), 0)) # line 215:
      #267::Relation.##267#268{Array{Int64,1}} = $(Expr(:new, Relation.##267#268{Array{Int64,1}}, :(t)))
      SSAValue(0) = #267::Relation.##267#268{Array{Int64,1}}
      $(Expr(:invoke, LambdaInfo for foreach(::Relation.##267#268{Array{Int64,1}}, ::Array{Int64,1}), :(Relation.foreach), SSAValue(0), :(xs))) # line 218:
      return (Base.arrayref)(t::Array{Int64,1},1)::Int64
  end::Int64
```

This is reflected in the emitted code - the non-boxed version has a constant 6 allocations whereas the boxed version allocates for each x in xs. 

To avoid having to create closures on each nexted iteration, I moved all the state variables to heap-allocated arrays at the top of the query.

``` julia
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols = (edges_xy[1], edges_xy[2], Int64[], edges_yz[1], edges_yz[2], Int64[], edges_xz[1], edges_xz[2], Int64[])
  los = [1 for _ in 1:length(cols)]
  ats = [1 for _ in 1:length(cols)]
  his = [length(cols[i])+1 for i in 1:length(cols)]
  count = [0]

  @time begin
    intersect(cols, los, ats, his, (1, 7)) do
      intersect(cols, los, ats, his, (2, 4)) do 
        intersect(cols, los, ats, his, (5, 8)) do
          count[1] += 1
        end 
      end 
    end
  end
  
  count[1]
end
```

Those nested closures still get created every time though (even though they are all identical) causing many many heap allocations. Rewriting like this fixed the problem:

``` julia 
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols = (edges_xy[1], edges_xy[2], Int64[], edges_yz[1], edges_yz[2], Int64[], edges_xz[1], edges_xz[2], Int64[])
  los = [1 for _ in 1:length(cols)]
  ats = [1 for _ in 1:length(cols)]
  his = [length(cols[i])+1 for i in 1:length(cols)]
  count = [0]
  
  cont4 = () -> count[1] += 1
  cont3 = () -> intersect(cont4, cols, los, ats, his, (5, 8))
  cont2 = () -> intersect(cont3, cols, los, ats, his, (2, 4))
  cont1 = () -> intersect(cont2, cols, los, ats, his, (1, 7))
  
  @time cont1()
  
  count[1]
end
```

Now `intersect` gets to be a normal function again.

``` julia 
function intersect(next, cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  @inbounds begin
    for ix in ixes
      ats[ix] = los[ix]
    end
    n = length(ixes)
    value = cols[ixes[n]][ats[ixes[n]]]
    fixed = 1
    while true 
      for ix in ixes
        if fixed == n
          for ix2 in ixes
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[ix2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          next()
        else 
          ats[ix] = gallop(cols[ix], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return 
        else 
          next_value = cols[ix][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end
```

This is only slightly slower than the macro version.

Belatedly, I realise that now that the state is kept outside the function I could just have avoided the closures all together:

``` julia 
function start_intersect(cols, los, ats, his, ixes)
  # assume los/his are valid 
  # los inclusive, his exclusive
  @inbounds begin
    for ix in ixes
      ats[ix] = los[ix]
    end
  end
end

function next_intersect(cols, los, ats, his, ixes)
  @inbounds begin
    fixed = 1
    n = length(ixes)
    value = cols[n][ats[ixes[n]]]
    while true 
      for c in 1:n
        ix = ixes[c]
        if fixed == n
          for c2 in 1:n
            ix2 = ixes[c2]
            los[ix2+1] = ats[ix2]
            his[ix2+1] = gallop(cols[c2], value, ats[ix2], his[ix2], <=)
            ats[ix2] = his[ix2+1]
          end
          return true
        else 
          ats[ix] = gallop(cols[c], value, ats[ix], his[ix], <)
        end
        if ats[ix] >= his[ix]
          return false
        else 
          next_value = cols[c][ats[ix]]
          fixed = (value == next_value) ? fixed+1 : 1
          value = next_value
        end
      end
    end
  end
end
```

Wish I had thought of that two days ago.

The setup is now kind of ugly, but the query compiler is going to be handling this anyway.

``` julia 
function f(edges_xy::Tuple{Vector{Int64}, Vector{Int64}}, edges_yz::Tuple{Vector{Int64}, Vector{Int64}}, edges_xz::Tuple{Vector{Int64}, Vector{Int64}}) 
  cols_x = [edges_xy[1], edges_xz[1]]
  cols_y = [edges_xy[2], edges_yz[1]]
  cols_z = [edges_yz[2], edges_xz[2]]
  ixes_x = [1,7]
  ixes_y = [2,4]
  ixes_z = [5,8]
  los = [1 for _ in 1:9]
  ats = [1 for _ in 1:9]
  his = [length(cols_x[1])+1 for i in 1:9]
  count = 0
  
  @time begin
    start_intersect(cols_x, los, ats, his, ixes_x)
    while next_intersect(cols_x, los, ats, his, ixes_x)
      x = cols_x[1][los[2]]
      start_intersect(cols_y, los, ats, his, ixes_y)
      while next_intersect(cols_y, los, ats, his, ixes_y)
        y = cols_y[1][los[3]]
        start_intersect(cols_z, los, ats, his, ixes_z)
        while next_intersect(cols_z, los, ats, his, ixes_z)
          z = cols_z[1][los[6]]
          # println((x,y,z))
          count += 1
        end
      end
    end
  end
  
  count
end
```

### 2016 Jul 31

I had some time this evening so I hashed out the core codegen.

To write code I have to figure out what data to compute, how to compute it, how to store it, in what order to compute it, how to organise the code, what to name things etc. I find that if I just sit down with an editor and try to do this all at once I spend a lot of time context switching, which manifests as these mental stack overflows where I just go blank for a while. 

Over the last year or so, I gradually started to batch these tasks together. I start by choosing a couple of examples and writing down the inputs and outputs. Then I sketch out what data will help me to get from input to output.

``` julia
q = quote 
  edge(a,b)
  edge(b,c)
  edge(c,a)
end

fieldnames(q)
q.head
q.args
q.args[2].head
q.args[2].args

a => (r1, c1), (r3, c2)
b => ...
c => ...

var order 

indexes
r1 => (1,2)
...

ixes 
r1, c1 => 1
r1, c2 => 2
r1, end => 3
...
r3, c2 => 7
r3, c1 => 8
r3, end => 9

cols = [r1, ...]
quicksort!((cols[1][2], cols[1][1]))
...
cols_a = (cols[1][2], cols[1][1])
ixes_a = (1, 7)
...
los, ats = 1
his = length(cols[r][c])
results = []

start_intersect 
while next_intersect 
  a = cols[1][2][los[3]]
  ...
     push!(results, (a,b,c))
end
```

Then I pick names and topo-sort the chunks of data. That whole plan then goes on one side of the screen and I can start cranking out code on the other side of the screen. The plan fills in my patchy short-term memory so the code flows smoothly. I didn't quite manage to type the compiler without hitting backspace, but it was close.

``` julia
function plan(query, variables)
  relations = [line.args[1] for line in query.args if line.head != :line]
  
  sources = Dict()
  for (clause, line) in enumerate(query.args) 
    if line.head != :line
      assert(line.head == :call)
      for (column, variable) in enumerate(line.args[2:end])
        assert(variable in variables)
        push!(get!(()->[], sources, variable), (clause,column))
      end
    end
  end
  
  sort_orders = Dict()
  for variable in variables 
    for (clause, column) in sources[variable]
      push!(get!(()->[], sort_orders, clause), column)
    end
  end
  
  ixes = Dict()
  next_ix = 1
  for (clause, columns) in sort_orders
    for column in columns
      ixes[(clause, column)] = next_ix
      next_ix += 1
    end
    ixes[(clause, :buffer)] = next_ix
    next_ix += 1
  end
  
  column_inits = Vector(length(ixes))
  for ((clause, column), ix) in ixes
    if column == :buffer
      column_inits[ix] = :()
    else
      clause_name = query.args[clause].args[1]
      column_inits[ix] = :(copy($(esc(clause_name))[$column]))
    end
  end
  
  sorts = []
  for (clause, columns) in sort_orders
    sort_ixes = [ixes[(clause, column)] for column in columns]
    sort_args = [:(columns[$ix]) for ix in sort_ixes]
    sort = :(quicksort!(tuple($(sort_args...))))
    push!(sorts, sort)
  end
  
  variable_inits = []
  for variable in variables 
    clauses_and_columns = sources[variable]
    variable_ixes = [ixes[(clause, column)] for (clause, column) in clauses_and_columns]
    variable_columns = [:(columns[$ix]) for ix in variable_ixes]
    variable_init = quote
      $(symbol("columns_", variable)) = [$(variable_columns...)]
      $(symbol("ixes_", variable)) = [$(variable_ixes...)]
    end
    push!(variable_inits, variable_init)
  end
  
  setup = quote
    columns = tuple($(column_inits...))
    $(sorts...)
    los = Int64[1 for i in 1:$(length(ixes))]
    ats = Int64[1 for i in 1:$(length(ixes))]
    his = Int64[length(columns[i]) for i in 1:$(length(ixes))]
    $(variable_inits...)
    results = []
  end
  
  function body(variable_ix)
    if variable_ix <= length(variables)
      variable = variables[variable_ix]
      variable_columns = symbol("columns_", variable)
      variable_ixes = symbol("ixes_", variable)
      result_column = ixes[sources[variable][1]]
      quote
        start_intersect($variable_columns, los, ats, his, $variable_ixes)
        while next_intersect($variable_columns, los, ats, his, $variable_ixes)
          $(esc(variable)) = columns[$result_column][los[$(result_column+1)]]
          $(body(variable_ix + 1))
        end
      end
    else 
      quote
        push!(results, tuple($([esc(variable) for variable in variables]...)))
      end 
    end
  end
          
  quote 
    $setup
    @time $(body(1))
    results
  end
end
```

With a crappy little macro we can now write the previous query as:

``` julia 
macro query(variables, query)
  plan(query, variables.args)
end

function f(edge) 
  @query([a,b,c], 
  begin
    edge(a,b)
    edge(b,c)
    edge(c,a)
  end)
end
```

Thats the basics. The next big steps are embedding an expression language and choosing the variable ordering automatically.  

EDIT: I found a little more time, so here is the chinook query from earlier in the year:

``` julia
album = read_columns("data/Album.csv", [Int64, String, Int64])
artist = read_columns("data/Artist.csv", [Int64, String])
track = read_columns("data/Track.csv", [Int64, String, Int64])
playlist_track = read_columns("data/PlaylistTrack.csv", [Int64, Int64])
playlist = read_columns("data/Playlist.csv", [Int64, String])

metal = read_columns("data/Metal.csv", [String])

function who_is_metal(album, artist, track, playlist_track, playlist, metal)
  @query([pn, p, t, al, a, an],
  begin
    metal(pn)
    playlist(p, pn)
    playlist_track(p, t)
    track(t, _, al)
    album(al, _, a)
    artist(a, an)
  end
  )[6]
end
```

Runs in 0.37ms, of which only about 0.01ms is solving the query and the rest is copying and sorting the inputs. My notes say the rust version took 0.8ms all together and SQLite took 1.2ms just to solve the query (both on an older machine). I won't bother benchmarking properly until the compiler is feature complete and I have tests, but looks like I'm inside my performance budget so far.

### 2016 Aug 1

Expressions just act as extra filters on results, so I can write things like:

``` julia
edge(a,b)
a < b
edge(b,c)
b < c
edge(c,a)
```

Evaluating them is pretty straightforward. I grab all the variables in the expression, assume any that aren't in the variable order are external constants, and figure out the earliest time when the remainder have been assigned.

``` julia
filters = [[] for _ in variables]
for clause in expression_clauses
  line = query.args[clause]
  callable_at = maximum(indexin(collect_variables(line), variables))
  push!(filters[callable_at], line)
end

function filter(filters, tail)
  if length(filters) == 0
    tail
  else 
    quote 
      if $(filters[1])
        $(filter(filters[2:end], tail))
      end
    end
  end
end

function body(variable_ix)
  ...
          $(filter(filters[variable_ix], body(variable_ix + 1)))
  ...
end
```

Equations are a bit trickier. An expression like `a == b + 1` could be treated as a filter on the results, but in many cases it would be much better to run it as soon as `b` is assigned, before wasting time generating many `a`s. On the other hand, that limits the compiler to variable orders where `b` comes before `a`, which may be inefficient. 

One of my core goals is to make performance predictable, so rather than deciding this in the compiler with some heuristic I'm going to have the programmer communicate intent directly. `a == b + 1` is a filter that will be run once `a` and `b` are both defined. `a = b + 1` is an assignment that forces `b` to be assigned before `a` and that will be run just before the intersection for `a`. In a true relational language this distinction wouldn't exist, but I want to be pragmatic for now.

``` julia
function assign(cols, los, ats, his, ixes, value)
  @inbounds begin
    n = length(ixes)
    for c in 1:n
      ix = ixes[c]
      los[ix+1] = gallop(cols[c], value, los[ix], his[ix], <)
      if los[ix+1] >= his[ix]
        return false
      end
      his[ix+1] = gallop(cols[c], value, los[ix+1], his[ix], <=)
    end
    return true
  end
end

function body(variable_ix)
  ...
    if haskey(assignment_clauses, variable)
      quote
        let $variable = $(assignment_clauses[variable])
          if assign($variable_columns, los, ats, his, $variable_ixes, $variable)
            # println($(repeat("  ", variable_ix)), $(string(variable)), "=", $variable)
            $tail
          end
        end
      end
    else
      ...
    end
  ...
end
```

Now we can do:

``` julia 
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end
```

What next? I have some ideas about variable ordering, but I need a lot more examples to see if they are realistic. Maybe projection/aggreation? I need to think a bit about how I want to handle that.

### 2016 Aug 4

Projection is really easy - we can just reuse the same building blocks:

``` julia 
metal = @query([pn, p, t, al, a, an],
begin
  pn = "Heavy Metal Classic"
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end
)

metal_projected = @query([an], 
begin
  metal(_, _, _, _, _, an)
end)
```

While I was doing that, I noticed that I'm returning columns of type `Any`. Fixing that is pretty tricky, because I don't actually know the type of the variables when I generate the query code. I'm relying on Julia's type inference, but type inference only happens after I generate code. I could wait until the first result to initialize the columns, but that doesn't work for queries with no results. 

Let's just work around it for now by allowing the user to specify the types in the query:

``` julia 
function plan(query, typed_variables)
  variables = []
  variable_types = []
  for typed_variable in typed_variables
    if isa(typed_variable, Symbol)
      push!(variables, typed_variable)
      push!(variable_types, :Any)
    elseif isa(typed_variable, Expr) && typed_variable.head == :(::)
      push!(variables, typed_variable.args[1])
      push!(variable_types, typed_variable.args[2])
    else 
      throw("Variable must be a symbol (with optional type annotation)")
    end
  end 
  ... 
     $(symbol("results_", variable)) = Vector{$(variable_type)}()
  ...
end
```

``` julia
@join([a,b,c], [a::Int64,b::Int64,c::Int64], 
begin
  edge(a,b)
  a < b
  edge(b,c)
  b < c
  edge(c,a)
end)
```

We can also do Yannakis-style queries:

``` julia 
function who_is_metal2(album, artist, track, playlist_track, playlist)
  i1 = @query([pn::String, p::Int64],
  begin
    pn = "Heavy Metal Classic"
    playlist(p, pn)
  end)
  i2 = @query([p::Int64, t::Int64],
  begin 
    i1(_, p)
    playlist_track(p, t)
  end)
  i3 = @query([t::Int64, al::Int64],
  begin 
    i2(_, t)
    track(t, _, al)
  end)
  i4 = @query([al::Int64, a::Int64],
  begin 
    i3(_, al)
    album(al, _, a)
  end)
  i5 = @query([a::Int64, an::String],
  begin 
    i4(_, a)
    artist(a, an)
  end)
  @query([an::String],
  begin
    i5(_, an)
  end)
end
```

On to aggregation. I have a planner that turns this:

``` julia
@join([pn],
[p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
(0.0,+,price::Float64),
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al, _, _, _, _, _, price)
end)
```

Into this:

``` 
begin  # /home/jamie/imp/src/Imp.jl, line 586:
    begin  # /home/jamie/imp/src/Imp.jl, line 411:
        begin  # /home/jamie/imp/src/Imp.jl, line 333:
            #11256#columns = tuple(copy(playlist_track[1]),copy(playlist_track[2]),(),copy(playlist[1]),copy(playlist[2]),(),copy(track[1]),copy(track[3]),copy(track[9]),()) # /home/jamie/imp/src/Imp.jl, line 334:
            quicksort!(tuple(#11256#columns[1],#11256#columns[2]))
            quicksort!(tuple(#11256#columns[4],#11256#columns[5]))
            quicksort!(tuple(#11256#columns[7],#11256#columns[8],#11256#columns[9])) # /home/jamie/imp/src/Imp.jl, line 335:
            #11257#los = Int64[1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 336:
            #11259#ats = Int64[1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 337:
            #11260#his = Int64[length(#11256#columns[#11258#i]) + 1 for #11258#i = 1:10] # /home/jamie/imp/src/Imp.jl, line 338:
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11261#columns_p = [#11256#columns[4],#11256#columns[1]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11262#ixes_p = [4,1] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11263#columns_pn = [#11256#columns[5]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11264#ixes_pn = [5] # /home/jamie/imp/src/Imp.jl, line 325:
                #11265#results_pn = Vector{String}()
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11266#columns_t = [#11256#columns[2],#11256#columns[7]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11267#ixes_t = [2,7] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11268#columns_al = [#11256#columns[8]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11269#ixes_al = [8] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end
            begin  # /home/jamie/imp/src/Imp.jl, line 323:
                #11270#columns_price = [#11256#columns[9]] # /home/jamie/imp/src/Imp.jl, line 324:
                #11271#ixes_price = [9] # /home/jamie/imp/src/Imp.jl, line 325:
                nothing
            end # /home/jamie/imp/src/Imp.jl, line 339:
            #11272#results_aggregate = Vector{Float64}()
        end # /home/jamie/imp/src/Imp.jl, line 412:
        begin  # /home/jamie/imp/src/Imp.jl, line 392:
            start_intersect(#11261#columns_p,#11257#los,#11259#ats,#11260#his,#11262#ixes_p) # /home/jamie/imp/src/Imp.jl, line 393:
            while next_intersect(#11261#columns_p,#11257#los,#11259#ats,#11260#his,#11262#ixes_p) # /home/jamie/imp/src/Imp.jl, line 394:
                #11273#p = (#11256#columns[4])[#11257#los[5]] # /home/jamie/imp/src/Imp.jl, line 395:
                begin  # /home/jamie/imp/src/Imp.jl, line 392:
                    start_intersect(#11263#columns_pn,#11257#los,#11259#ats,#11260#his,#11264#ixes_pn) # /home/jamie/imp/src/Imp.jl, line 393:
                    while next_intersect(#11263#columns_pn,#11257#los,#11259#ats,#11260#his,#11264#ixes_pn) # /home/jamie/imp/src/Imp.jl, line 394:
                        #11274#pn = (#11256#columns[5])[#11257#los[6]] # /home/jamie/imp/src/Imp.jl, line 395:
                        begin  # /home/jamie/imp/src/Imp.jl, line 364:
                            #11275#aggregate = 0.0 # /home/jamie/imp/src/Imp.jl, line 365:
                            begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                start_intersect(#11266#columns_t,#11257#los,#11259#ats,#11260#his,#11267#ixes_t) # /home/jamie/imp/src/Imp.jl, line 393:
                                while next_intersect(#11266#columns_t,#11257#los,#11259#ats,#11260#his,#11267#ixes_t) # /home/jamie/imp/src/Imp.jl, line 394:
                                    #11276#t = (#11256#columns[2])[#11257#los[3]] # /home/jamie/imp/src/Imp.jl, line 395:
                                    begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                        start_intersect(#11268#columns_al,#11257#los,#11259#ats,#11260#his,#11269#ixes_al) # /home/jamie/imp/src/Imp.jl, line 393:
                                        while next_intersect(#11268#columns_al,#11257#los,#11259#ats,#11260#his,#11269#ixes_al) # /home/jamie/imp/src/Imp.jl, line 394:
                                            #11277#al = (#11256#columns[8])[#11257#los[9]] # /home/jamie/imp/src/Imp.jl, line 395:
                                            begin  # /home/jamie/imp/src/Imp.jl, line 392:
                                                start_intersect(#11270#columns_price,#11257#los,#11259#ats,#11260#his,#11271#ixes_price) # /home/jamie/imp/src/Imp.jl, line 393:
                                                while next_intersect(#11270#columns_price,#11257#los,#11259#ats,#11260#his,#11271#ixes_price) # /home/jamie/imp/src/Imp.jl, line 394:
                                                    #11278#price = (#11256#columns[9])[#11257#los[10]] # /home/jamie/imp/src/Imp.jl, line 395:
                                                    #11275#aggregate = #11275#aggregate + #11278#price::Float64
                                                end
                                            end
                                        end
                                    end
                                end
                            end # /home/jamie/imp/src/Imp.jl, line 366:
                            if #11275#aggregate != 0.0 # /home/jamie/imp/src/Imp.jl, line 367:
                                push!(#11265#results_pn,#11274#pn) # /home/jamie/imp/src/Imp.jl, line 370:
                                push!(#11272#results_aggregate,#11275#aggregate)
                            end
                        end
                    end
                end
            end
        end # /home/jamie/imp/src/Imp.jl, line 413:
        tuple(#11265#results_pn,#11272#results_aggregate)
    end
end
```

It only aggregates after the last variable in the ordering that is returned, so if I want to aggregate over variables that are earlier in the ordering I need to apply another join to the result.

``` julia 
result = @join([p, pn, t],
[p::Int64, pn::String, t::Int64, al::Int64, price::Float64],
(0.0,+,price::Float64),
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al, _, _, _, _, _, price)
end)

@join([t],
[t::Int64, p::Int64, pn::String, price::Float64],
(0.0,+,price::Float64),
begin
  result(p, pn, t, price)
end)
```

I want to wrap that up in another ugly macro but right now I'm just flailing and nothing works. Tomorrow...

### 2016 Aug 5

Ok, I finally got this nailed down. There were a bunch of little things I had to fix.

The inputs to queries are sets, but the query effectively projects out the columns it cares about. That didn't matter before, but for aggregates we care about the number of results, not just the values. Now I count the number of repeated solutions:

``` julia 
repeats = 1
for buffer_ix in buffer_ixes
  repeats = :($repeats * (his[$buffer_ix] - los[$buffer_ix]))
end
body = :(aggregate = $(aggregate_add)(aggregate, $aggregate_expr, $repeats))
```

The `aggregate_add` is now required to take a third argument that gives an exponent to the operation.

``` julia 
@inline add_exp(a, b, n) = a + (b * n)
@inline mul_exp(a, b, n) = a * (b ^ n)
```

I split the old `plan` into `analyse` and `plan_join` so that I could reuse the parts:

``` julia
function plan_query(returned_variables, typed_variables, aggregate, query)
  aggregate_zero, aggregate_add, aggregate_expr = aggregate
  aggregate_type, variables, variable_types, return_ix = analyse(returned_variables, typed_variables, aggregate)
  join = plan_join(returned_variables, aggregate, aggregate_type, variables, variable_types, return_ix, query)
  project_variables = Any[variable for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  project_variable_types = Any[variable_type for (variable, variable_type) in zip(variables, variable_types) if variable in returned_variables]
  push!(project_variables, :prev_aggregate)
  push!(project_variable_types, aggregate_type)
  project_aggregate = [aggregate_zero, aggregate_add, :prev_aggregate]
  project_query = quote 
    intermediate($(project_variables...))
  end
  project_return_ix = length(returned_variables) + 1
  project = plan_join(returned_variables, project_aggregate, aggregate_type, project_variables, project_variable_types, project_return_ix, project_query)
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end
```

The default aggregate just counts the number of results:

``` julia 
macro query(returned_variables, typed_variables, query)
  :(@query($returned_variables, $typed_variables, (0, add_exp, 1::Int64), $query))
end

macro query(returned_variables, typed_variables, aggregate, query)
  plan_query(returned_variables.args, typed_variables.args, aggregate.args, query)
end
```

Now we can ask questions like how many times each artist appears on a given playlist:

``` julia 
@query([pn, an],
[pn::String, p::Int64, t::Int64, al::Int64, a::Int64, an::String],
begin
  playlist(p, pn)
  playlist_track(p, t)
  track(t, _, al)
  album(al, _, a)
  artist(a, an)
end)
```

I've been putting off dealing with hygiene in the planner, but I spent about an hour on a hygiene bug today so I suppose I should move that up the todo list. 

I also have to do something about caching sorted relations, and then I think I have enough to try the [Join Order Benchmark](http://www.vldb.org/pvldb/vol9/p204-leis.pdf). It uses the IMDB dataset (which is about 3.6GB of strings) and asks questions such as:

``` sql
SELECT MIN(cn1.name) AS first_company,
       MIN(cn2.name) AS second_company,
       MIN(mi_idx1.info) AS first_rating,
       MIN(mi_idx2.info) AS second_rating,
       MIN(t1.title) AS first_movie,
       MIN(t2.title) AS second_movie
FROM company_name AS cn1,
     company_name AS cn2,
     info_type AS it1,
     info_type AS it2,
     kind_type AS kt1,
     kind_type AS kt2,
     link_type AS lt,
     movie_companies AS mc1,
     movie_companies AS mc2,
     movie_info_idx AS mi_idx1,
     movie_info_idx AS mi_idx2,
     movie_link AS ml,
     title AS t1,
     title AS t2
WHERE cn1.country_code != '[us]'
  AND it1.info = 'rating'
  AND it2.info = 'rating'
  AND kt1.kind IN ('tv series',
                   'episode')
  AND kt2.kind IN ('tv series',
                   'episode')
  AND lt.link IN ('sequel',
                  'follows',
                  'followed by')
  AND mi_idx2.info < '3.5'
  AND t2.production_year BETWEEN 2000 AND 2010
  AND lt.id = ml.link_type_id
  AND t1.id = ml.movie_id
  AND t2.id = ml.linked_movie_id
  AND it1.id = mi_idx1.info_type_id
  AND t1.id = mi_idx1.movie_id
  AND kt1.id = t1.kind_id
  AND cn1.id = mc1.company_id
  AND t1.id = mc1.movie_id
  AND ml.movie_id = mi_idx1.movie_id
  AND ml.movie_id = mc1.movie_id
  AND mi_idx1.movie_id = mc1.movie_id
  AND it2.id = mi_idx2.info_type_id
  AND t2.id = mi_idx2.movie_id
  AND kt2.id = t2.kind_id
  AND cn2.id = mc2.company_id
  AND t2.id = mc2.movie_id
  AND ml.linked_movie_id = mi_idx2.movie_id
  AND ml.linked_movie_id = mc2.movie_id
  AND mi_idx2.movie_id = mc2.movie_id;
``` 

Or:

``` sql 
SELECT MIN(mc.note) AS production_note,
       MIN(t.title) AS movie_title,
       MIN(t.production_year) AS movie_year
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t
WHERE ct.kind = 'production companies'
  AND it.info = 'top 250 rank'
  AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
  AND (mc.note LIKE '%(co-production)%'
       OR mc.note LIKE '%(presents)%')
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND t.id = mi_idx.movie_id
  AND mc.movie_id = mi_idx.movie_id
  AND it.id = mi_idx.info_type_id;
```

### 2016 Aug 6

Ok, so we're gonna store the columns in some object that caches various sort orders.

``` julia
type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
end

function Relation{T}(columns::T)
  Relation(columns, Dict{Vector{Int64},T}())
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  get!(relation.indexes, order) do
    index::T = tuple([(ix in order) ? copy(column) : Vector{eltype(column)}() for (ix, column) in enumerate(relation.columns)]...)
    quicksort!(tuple([index[ix] for ix in order]...))
    index
  end
end
```

We're not being very smart about sharing indexes. If we request [1,2] and there is already an index for [1,2,3] we could just return that, but instead we make a new and pointless index. I'll fix that one day.

When we create a new index, we sort the columns in the order specified but return them in the original order, with any unsorted columns emptied out. This ensures that the return type of the function doesn't depend on the order. Eventually I'll get around to wrapping each query in a function to create a dispatch point and then it won't matter, but for now this helps Julia correctly infer types downstream.

I currently have IMDbPY inserting data into postgres. That reportedly takes about 8 hours (although the hardware described in the readme is anaemic) but as a side-effect it will spit out csv versions of all the tables that I can use in Imp.

One hour later:

```
loading CSV files into the database
 * LOADING CSV FILE imdb/csv/imdb/csv/complete_cast.csv...
ERROR: unable to import CSV file imdb/csv/imdb/csv/complete_cast.csv: could not open file "imdb/csv/imdb/csv/complete_cast.csv" for reading: No such file or directory
```

It created all the csv files just fine and then somehow mangled the filenames before trying to load them. Trying to just run the csv->db step ran into a different set of errors (which I lost by closing the wrong window :), so let's run it again with the row-by-row insert option. 

In the meantime, I tried to load the livejournal dataset into Julia, which caused the atom plugin to blowup:

``` 
/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16
RangeError: Invalid string length
    at Socket.<anonymous> (/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16:26)
    at emitOne (events.js:77:13)
    at Socket.emit (events.js:169:7)
    at readableAddChunk (_stream_readable.js:146:16)
    at Socket.Readable.push (_stream_readable.js:110:10)
    at TCP.onread (net.js:523:20)
```

Maybe the problem is that it displays relations by printing the entire contents, rather than just showing the head and tail like it does with large arrays. I poked around inside the source code, found the function that controls rendering and added an override for relations:

``` julia
import Atom
function Atom.render(editor::Atom.Editor, relation::Relation)
  Atom.render(editor, relation.columns)
end
```

I checked with a smaller relation that it does affect the rendering. Does it fix the bug? Nope:

```
/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16
RangeError: Invalid string length
    at Socket.<anonymous> (/home/jamie/.atom/packages/julia-client/lib/connection/local.coffee:16:26)
    at emitOne (events.js:77:13)
    at Socket.emit (events.js:169:7)
    at readableAddChunk (_stream_readable.js:146:16)
    at Socket.Readable.push (_stream_readable.js:110:10)
    at TCP.onread (net.js:523:20)
```

Debugging by guessing - not a thing.

### 2016 Aug 8

Still working through various problems getting IMDbPY to work.

``` 
ERROR: unable to import CSV file /home/jamie/imdb/csv/movie_link.csv: null value in column "movie_id" violates not-null constraint
DETAIL:  Failing row contains (15021, null, 101237, 12).
CONTEXT:  COPY movie_link, line 15021: "15021,NULL,101237,12"

 * LOADING CSV FILE /home/jamie/imdb/csv/char_name.csv...
# TIME loadCSVFiles() : 6min, 24sec (wall) 0min, 0sec (user) 0min, 0sec (system)
# TIME TOTAL TIME TO INSERT/WRITE DATA : 28min, 42sec (wall) 21min, 52sec (user) 0min, 23sec (system)
building database indexes (this may take a while)
# TIME createIndexes() : 8min, 44sec (wall) 0min, 0sec (user) 0min, 0sec (system)
adding foreign keys (this may take a while)
ERROR caught exception creating a foreign key: insert or update on table "aka_title" violates foreign key constraint "movie_id_exists"
DETAIL:  Key (movie_id)=(0) is not present in table "title".
```

Instead, I found a [link](http://homepages.cwi.nl/~boncz/job/imdb.tgz) to the CSV files the authors of the paper used, and loaded those directly into postgres myself. Which took about 5 minutes.

```
\i job/schema.sql

\copy aka_name from 'imdb/aka_name.csv' csv escape '\'
\copy aka_title from 'imdb/aka_title.csv' csv escape '\'
\copy cast_info from 'imdb/cast_info.csv' csv escape '\'
\copy char_name from 'imdb/char_name.csv' csv escape '\'
\copy comp_cast_type from 'imdb/comp_cast_type.csv' csv escape '\'
\copy company_name from 'imdb/company_name.csv' csv escape '\'
\copy company_type from 'imdb/company_type.csv' csv escape '\'
\copy complete_cast from 'imdb/complete_cast.csv' csv escape '\'
\copy info_type from 'imdb/info_type.csv' csv escape '\'
\copy keyword from 'imdb/keyword.csv' csv escape '\'
\copy kind_type from 'imdb/kind_type.csv' csv escape '\'
\copy link_type from 'imdb/link_type.csv' csv escape '\'
\copy movie_companies from 'imdb/movie_companies.csv' csv escape '\'
\copy movie_info from 'imdb/movie_info.csv' csv escape '\'
\copy movie_info_idx from 'imdb/movie_info_idx.csv' csv escape '\'
\copy movie_keyword from 'imdb/movie_keyword.csv' csv escape '\'
\copy movie_link from 'imdb/movie_link.csv' csv escape '\'
\copy name from 'imdb/name.csv' csv escape '\'
\copy person_info from 'imdb/person_info.csv' csv escape '\'
\copy role_type from 'imdb/role_type.csv' csv escape '\'
\copy title from 'imdb/title.csv' csv escape '\'

\i job/fkindexes.sql
```

Now let's dump the schema in a way that's easy for Imp to read:

```
copy (select table_name, ordinal_position, column_name, data_type from information_schema.columns) to '/home/jamie/imp/data/job_schema.csv' with csv delimiter ',';
```

Eugh, and the csv files themselves have backslash-escaped strings that Julia can't read, so let's re-export those.

```
\copy aka_name to 'job/aka_name.csv' csv escape '"'
\copy aka_title to 'job/aka_title.csv' csv escape '"'
\copy cast_info to 'job/cast_info.csv' csv escape '"'
\copy char_name to 'job/char_name.csv' csv escape '"'
\copy comp_cast_type to 'job/comp_cast_type.csv' csv escape '"'
\copy company_name to 'job/company_name.csv' csv escape '"'
\copy company_type to 'job/company_type.csv' csv escape '"'
\copy complete_cast to 'job/complete_cast.csv' csv escape '"'
\copy info_type to 'job/info_type.csv' csv escape '"'
\copy keyword to 'job/keyword.csv' csv escape '"'
\copy kind_type to 'job/kind_type.csv' csv escape '"'
\copy link_type to 'job/link_type.csv' csv escape '"'
\copy movie_companies to 'job/movie_companies.csv' csv escape '"'
\copy movie_info to 'job/movie_info.csv' csv escape '"'
\copy movie_info_idx to 'job/movie_info_idx.csv' csv escape '"'
\copy movie_keyword to 'job/movie_keyword.csv' csv escape '"'
\copy movie_link to 'job/movie_link.csv' csv escape '"'
\copy name to 'job/name.csv' csv escape '"'
\copy person_info to 'job/person_info.csv' csv escape '"'
\copy role_type to 'job/role_type.csv' csv escape '"'
\copy title to 'job/title.csv' csv escape '"' 
```

Let's grab the first query from the benchmark and get a feel for long it takes.

```
postgres=# prepare q1a as SELECT MIN(mc.note) AS production_note, MIN(t.title) AS movie_title, MIN(t.production_year) AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND mc.note  not like '%(as Metro-Goldwyn-Mayer Pictures)%' and (mc.note like '%(co-production)%' or mc.note like '%(presents)%') AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id;
ERROR:  prepared statement "q1a" already exists
Time: 0.356 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.213 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.578 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.109 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.317 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.187 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.794 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.536 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 5.981 ms
postgres=# execute q1a;
             production_note             |    movie_title     | movie_year 
-----------------------------------------+--------------------+------------
 (as Indo-British Films Ltd.) (presents) | A Clockwork Orange |       1934
(1 row)

Time: 6.122 ms
```

So around 6ms. 

```
postgres=# EXPLAIN ANALYZE execute q1a;
                                                                                 QUERY PLAN                                                                                 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Aggregate  (cost=30010.08..30010.09 rows=1 width=45) (actual time=5.704..5.704 rows=1 loops=1)
   ->  Nested Loop  (cost=6482.03..30010.06 rows=3 width=45) (actual time=0.098..5.658 rows=142 loops=1)
         Join Filter: (mc.movie_id = t.id)
         ->  Hash Join  (cost=6481.60..30008.29 rows=3 width=32) (actual time=0.092..5.225 rows=142 loops=1)
               Hash Cond: (mc.company_type_id = ct.id)
               ->  Nested Loop  (cost=6462.68..29987.21 rows=566 width=36) (actual time=0.071..5.173 rows=147 loops=1)
                     ->  Nested Loop  (cost=6462.25..22168.36 rows=12213 width=4) (actual time=0.036..0.091 rows=250 loops=1)
                           ->  Seq Scan on info_type it  (cost=0.00..2.41 rows=1 width=4) (actual time=0.012..0.013 rows=1 loops=1)
                                 Filter: ((info)::text = 'top 250 rank'::text)
                                 Rows Removed by Filter: 112
                           ->  Bitmap Heap Scan on movie_info_idx mi_idx  (cost=6462.25..18715.86 rows=345009 width=8) (actual time=0.022..0.036 rows=250 loops=1)
                                 Recheck Cond: (info_type_id = it.id)
                                 Heap Blocks: exact=2
                                 ->  Bitmap Index Scan on info_type_id_movie_info_idx  (cost=0.00..6375.99 rows=345009 width=0) (actual time=0.015..0.015 rows=250 loops=1)
                                       Index Cond: (info_type_id = it.id)
                     ->  Index Scan using movie_id_movie_companies on movie_companies mc  (cost=0.43..0.63 rows=1 width=32) (actual time=0.020..0.020 rows=1 loops=250)
                           Index Cond: (movie_id = mi_idx.movie_id)
                           Filter: ((note !~~ '%(as Metro-Goldwyn-Mayer Pictures)%'::text) AND ((note ~~ '%(co-production)%'::text) OR (note ~~ '%(presents)%'::text)))
                           Rows Removed by Filter: 33
               ->  Hash  (cost=18.88..18.88 rows=4 width=4) (actual time=0.017..0.017 rows=1 loops=1)
                     Buckets: 1024  Batches: 1  Memory Usage: 9kB
                     ->  Seq Scan on company_type ct  (cost=0.00..18.88 rows=4 width=4) (actual time=0.011..0.011 rows=1 loops=1)
                           Filter: ((kind)::text = 'production companies'::text)
                           Rows Removed by Filter: 3
         ->  Index Scan using title_pkey on title t  (cost=0.43..0.58 rows=1 width=25) (actual time=0.003..0.003 rows=1 loops=142)
               Index Cond: (id = mi_idx.movie_id)
 Execution time: 5.757 ms
```

The overwhelming majority of the time is attributed to the final aggregate, which is weird. I don't know much about how it calculates these times, but I would expect producing the data to take at lesat as much time as reducing it.

Let's get some data into Imp!

``` julia
function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  tables = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(tables, table_name, []), (ix, column_name, column_type))
  end
  relations = []
  names = []
  for (table_name, columns) in tables
    if isfile("../job/$(table_name).csv")
      rows = readdlm(open("../job/$(table_name).csv"), ',', header=false, quotes=true, comments=false)
      n = size(rows)[1]
      ids = Int64[rows[r,1] for r in 1:n]
      push!(names, symbol(table_name))
      push!(relations, Relation((ids,)))
      for (ix, column_name, column_type) in columns[2:end]
        @show table_name ix column_name column_type
        if column_type == "integer"
          ids = Int64[]
          column = Int64[]
          for r in 1:n
            if !(rows[r, ix] in ("", "null", "NULL"))
              push!(ids, rows[r, 1])
              push!(column, rows[r, ix])
            end
          end
        else
          ids = Int64[]
          column = String[]
          for r in 1:n
            if !(rows[r, ix] in ("", "null", "NULL"))
              push!(ids, rows[r, 1])
              push!(column, string(rows[r, ix]))
            end
          end
        end
        push!(names, symbol(table_name, "_", column_name))
        push!(relations, Relation((ids, column)))
      end
    end
  end
  (names, relations)
end
```

This reads the schema I dumped out of postgres and builds a normalized set of relations (taking advantage of the fact that every table in the dataset has a single integer as it's primary key). I'm normalizing it this way to avoid having to represent with null entries directly. Possible future feature.

I'm using the stdlib csv reading function, which generates a single big array containing all the data, meaning that if there are any strings then all the integers have to be boxed too and everything goes to poop.  

The csv reading code also returns `SubString`s - pointers to slices of the mmaped file - rather than allocating individual strings. But this seems unrealistic - I don't actually expect real-world data to arrive all in one nice contiguous file. So I'm reallocating them all as individual strings.

All of this means that loading the data takes FOREVER, but the final representation is pretty sensible. Later I'll have to find a faster way of doing this. Maybe DataFrames.jl is better?

``` julia
using DataFrames

function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict()
  table_column_types = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), (column_type == "integer" ? Int64 : String))
  end
  relations = []
  names = []
  for (table_name, column_names) in table_column_names
    if isfile("../job/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      frame = readtable(open("../imdb/$(table_name).csv"), header=false, eltypes=column_types)
      n = length(frame[1])
      ids = copy(frame[1].data)
      for (ix, (column_name, column_type)) in enumerate(zip(column_names, column_types))
        @show table_name ix column_name column_type
        data_array = frame[ix]
        if ix == 1
          push!(names, symbol(table_name))
          push!(relations, Relation((ids,)))
        else
          column_ids = Int64[id for (ix, id) in enumerate(ids) if !(data_array.na[ix])]
          local column
          if isa(data_array, DataArray{Int64})
            let data::Vector{Int64} = data_array.data
              column = Int64[d for (ix, d) in enumerate(data) if !(data_array.na[ix])]
            end
          elseif isa(data_array, DataArray{String})
            let data::Vector{String} = data_array.data
              column = String[d for (ix, d) in enumerate(data_array.data) if !(data_array.na[ix])]
            end
          end
          push!(names, symbol(table_name, "_", column_name))
          push!(relations, Relation((column_ids, column)))
        end
      end
    end
  end
  (names, relations)
end
```

Woah, way way faster. 

Weirdly, unpacking the results into individual variable names blows up with an out-of-memory error.

``` julia 
person_info,person_info_person_id,person_info_info_type_id,person_info_info,person_info_note,title,title_title,title_imdb_index,title_kind_id,title_production_year,title_imdb_id,title_phonetic_code,title_episode_of_id,title_season_nr,title_episode_nr,title_series_years,title_md5sum,link_type,link_type_link,cast_info,cast_info_person_id,cast_info_movie_id,cast_info_person_role_id,cast_info_note,cast_info_nr_order,cast_info_role_id,movie_info_idx,movie_info_idx_movie_id,movie_info_idx_info_type_id,movie_info_idx_info,movie_info_idx_note,name,name_name,name_imdb_index,name_imdb_id,name_gender,name_name_pcode_cf,name_name_pcode_nf,name_surname_pcode,name_md5sum,info_type,info_type_info,aka_name,aka_name_person_id,aka_name_name,aka_name_imdb_index,aka_name_name_pcode_cf,aka_name_name_pcode_nf,aka_name_surname_pcode,aka_name_md5sum,movie_info,movie_info_movie_id,movie_info_info_type_id,movie_info_info,movie_info_note,role_type,role_type_role,aka_title,aka_title_movie_id,aka_title_title,aka_title_imdb_index,aka_title_kind_id,aka_title_production_year,aka_title_phonetic_code,aka_title_episode_of_id,aka_title_season_nr,aka_title_episode_nr,aka_title_note,aka_title_md5sum,complete_cast,complete_cast_movie_id,complete_cast_subject_id,complete_cast_status_id,movie_keyword,movie_keyword_movie_id,movie_keyword_keyword_id,kind_type,kind_type_kind,movie_link,movie_link_movie_id,movie_link_linked_movie_id,movie_link_link_type_id,company_name,company_name_name,company_name_country_code,company_name_imdb_id,company_name_name_pcode_nf,company_name_name_pcode_sf,company_name_md5sum,keyword,keyword_keyword,keyword_phonetic_code,comp_cast_type,comp_cast_type_kind,char_name,char_name_name,char_name_imdb_index,char_name_imdb_id,char_name_name_pcode_nf,char_name_surname_pcode,char_name_md5sum,movie_companies,movie_companies_movie_id,movie_companies_company_id,movie_companies_company_type_id,movie_companies_note,company_type,company_type_kind = relations
```

I have no idea why. At a guess, unpacking 100-odd variables at once triggers some weird corner case in the compiler.

But now the julia process is dead and I have to load all that data into memory again. Sigh...

The reason I wanted to unpack everything is that the query compiler currently can't handle non-symbol relation names eg `person_info_person_id(p, pi)` works but `db[:person_info, :person_id](p, pi)` does not. But I can fix that pretty easily - let's finally get around to wrapping the query in a function.

``` julia
function plan(...)
  ...
  quote 
    # TODO pass through any external vars too to avoid closure boxing grossness
    function query($([symbol("relation_", clause) for clause in relation_clauses]...))
      $setup
      $body
      Relation(tuple($([symbol("results_", variable) for variable in returned_variables]...), results_aggregate))
    end
    query($([esc(query.args[clause].args[1]) for clause in relation_clauses]...))
  end
end
```

So the generated code will look like:

``` julia 
function query(relation_1, ...)
  ...
end
query(db[:person_info, :person_id], ...)
```

Now I'll load the imdb data into a dict of relations, and then try to serialize it so I don't have to do it again:

``` julia 
job = @time read_job()

open("../job/imp.bin", "w") do f
    @time serialize(f, job)
end

# 743.572276 seconds (4.92 G allocations: 139.663 GB, 65.42% gc time)
# 42.551220 seconds (92.15 M allocations: 1.384 GB, 7.74% gc time)
```

140GB of temporary allocations. Something in there is still a mess.

``` julia 
job = @time deserialize(open("../job/imp.bin"))
# 700.359796 seconds (943.00 M allocations: 50.969 GB, 78.30% gc time)
# OutOfMemoryError()
```

So that's weird. It made a big mess of allocations deserializing the data, finished, then about 3 seconds later threw an out of memory error. 

Later, trying to rebuild the dataset, Julia dies with:

```
Julia has stopped: null, SIGKILL
```

This is frustrating. Reading a 6gb dataset on a machine with 32gb of ram should not be difficult.

After several attempts, JLD manages to both save and load the dataset without exploding, although never both sequentially. After gc, top shows:

```
13908 jamie     20   0 22.560g 0.017t  64752 S  61.4 54.2   2:03.90 julia
```

I asked to see the results (bearing in mind that the representation is truncated) and...

``` 
OutOfMemoryError()
 in resize!(::Array{UInt8,1}, ::UInt64) at ./array.jl:470
 in ensureroom at ./iobuffer.jl:194 [inlined]
 in unsafe_write(::Base.AbstractIOBuffer{Array{UInt8,1}}, ::Ptr{UInt8}, ::UInt64) at ./iobuffer.jl:275
 in write(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{UInt8,1}) at ./io.jl:161
 in show at ./show.jl:234 [inlined]
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String, ::String, ::Bool, ::Int64, ::Int64) at ./show.jl:318
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String, ::String, ::Bool) at ./show.jl:300
 in show_vector(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::String, ::String) at ./show.jl:1666
 in #showarray#252(::Bool, ::Function, ::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Array{Int64,1}, ::Bool) at ./show.jl:1585
 in show_delim_array(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Tuple{Array{Int64,1},Array{Int64,1}}, ::Char, ::Char, ::Char, ::Bool, ::Int64, ::Int64) at ./show.jl:355
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Tuple{Array{Int64,1},Array{Int64,1}}) at ./show.jl:376
 in show_default(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Any) at ./show.jl:130
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::Any) at ./show.jl:116
 in show(::IOContext{Base.AbstractIOBuffer{Array{UInt8,1}}}, ::MIME{Symbol("text/plain")}, ::Dict{Any,Any}) at ./replutil.jl:94
 in verbose_show(::Base.AbstractIOBuffer{Array{UInt8,1}}, ::MIME{Symbol("text/plain")}, ::Dict{Any,Any}) at ./multimedia.jl:50
 in #sprint#226(::Void, ::Function, ::Int64, ::Function, ::MIME{Symbol("text/plain")}, ::Vararg{Any,N}) at ./strings/io.jl:37
 in Type at /home/jamie/.julia/v0.5/Atom/src/display/view.jl:78 [inlined]
 in Type at /home/jamie/.julia/v0.5/Atom/src/display/view.jl:79 [inlined]
 in render(::Atom.Editor, ::Dict{Any,Any}) at /home/jamie/.julia/v0.5/Atom/src/display/display.jl:23
 in (::Atom.##91#95)(::Dict{String,Any}) at /home/jamie/.julia/v0.5/Atom/src/eval.jl:62
 in handlemsg(::Dict{String,Any}, ::Dict{String,Any}, ::Vararg{Dict{String,Any},N}) at /home/jamie/.julia/v0.5/Atom/src/comm.jl:71
 in (::Atom.##5#8)() at ./event.jl:46
```

But if I treat it with kid gloves and never ask to see the actual result, I can write my first tiny query against the dataset:

``` julia
@query([cid, cn],
[cid::Int64, cn::String],
begin 
  job["company_name", "name"](cid, cn)
end)
```

That works fine.

But if I wrap it in a function and run the function I get a bounds error (which takes a long time to generate because Julia prints the entire relation in the error). I think inside a function scope, functions are all defined up top, but globally the definitions are executed sequentially. So if the function names collide, behaviour in each scope is different. I added a counter just uniquefies each function name and the problem went away.

Let's have a go at query 1a.

``` julia 
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

function f()
  @query([],
  [ct_kind::String, ct_id::Int64, mc_id::Int64, mc_note::String, t_id::Int64, mii_id::Int64, it_id::Int64, it_info::String, t_production_year::Int64],
  (3000, min_exp, t_production_year),
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

# first run (indexing + compilation)
# 7.278131 seconds (479.39 k allocations: 192.071 MB, 82.77% gc time)

# second run
# 0.118113 seconds (292.96 k allocations: 4.476 MB)
```

118ms. Not going to knock postgres off any pedastals just yet. 

I want to know what's going on with those allocations. There should barely be any. I squelched a few type-inference failures but it didn't change the number of allocations at all, which is weird.

### 2016 Aug 09

Looks like `ismatch` causes a single heap allocation on each call. So nothing wrong with my compiler.

Let's pick a query with no regexes.

``` julia
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
  @query([],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  ("zzzzzzzzzzz", min_exp, title::String),
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

# 1.998575 seconds (142.43 k allocations: 44.412 MB, 63.25% gc time)
# 0.126513 seconds (108 allocations: 7.250 KB)
# 0.125623 seconds (108 allocations: 7.250 KB)
```

``` 
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 2388.770 ms
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 449.339 ms
postgres=# execute q2a;
       movie_title        
--------------------------
 008 - Agent wider Willen
(1 row)

Time: 449.340 ms
```

Not worth reading too much into that, because I'm getting a different answer to postgres.

Man, where do I even start debugging something like that. I guess, break the query down into pieces and find where it starts to diverge.

``` julia
function q2a()
  @query([cnit, k_id],
  [cnit::String, k_id::Int64], #, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    # job["movie_keyword", "movie_id"](mk_id, t_id)
    # job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([mk_id],
  [cnit::String, k_id::Int64, mk_id::Int64], # t_id::Int64, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    # job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([t_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64], #, mc_id::Int64, cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    # job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([mc_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64], # cn_id::Int64, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    # job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

function q2a()
  @query([cn_id],
  [cnit::String, k_id::Int64, mk_id::Int64, t_id::Int64, mc_id::Int64, cn_id::Int64], #, de::String, title::String],
  begin
    # de = "[de]"
    # job["company_name", "country_code"](cn_id, de) 
    cnit = "character-name-in-title"
    job["keyword", "keyword"](k_id, cnit)
    job["movie_companies", "company_id"](mc_id, cn_id)
    job["movie_companies", "movie_id"](mc_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    # job["title", "title"](t_id, title)
  end)
end

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
```

Eugh, that was tedious. Turns out that the query is correct, but `min` in sql uses a different ordering and I forget to use `count(distinct ...)` when I double-checked the total results.

So let's have sql return the distinct count and Imp return the column of results, which seems roughly fair.

```
postgres=# execute q2a_all;
Time: 443.249 ms
postgres=# prepare q2a_distinct as select count(distinct t.title) AS movie_title FROM company_name AS cn, keyword AS k, movie_companies AS mc, movie_keyword AS mk, title AS t WHERE cn.country_code ='[de]' AND k.keyword ='character-name-in-title' AND cn.id = mc.company_id AND mc.movie_id = t.id AND t.id = mk.movie_id AND mk.keyword_id = k.id AND mc.movie_id = mk.movie_id;
PREPARE
Time: 0.468 ms
postgres=# execute q2a_distinct
postgres-# ;
 movie_title 
-------------
        4127
(1 row)

Time: 455.719 ms
postgres=# execute q2a_distinct;
 movie_title 
-------------
        4127
(1 row)

Time: 450.318 ms
postgres=# execute q2a_distinct;
 movie_title 
-------------
        4127
(1 row)

Time: 441.992 ms
```

``` julia
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

# 0.128545 seconds (197 allocations: 646.297 KB)
# 0.140465 seconds (197 allocations: 646.297 KB)
# 0.138893 seconds (197 allocations: 646.297 KB)
```

Score. Faster than postgres on q2a, with the first variable ordering I tried.

Let's go back to q1a and steal the execution plan from postgres.

```
postgres=# prepare q1a_distinct as SELECT count(distinct t.production_year) AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND mc.note  not like '%(as Metro-Goldwyn-Mayer Pictures)%' and (mc.note like '%(co-production)%' or mc.note like '%(presents)%') AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id;
ERROR:  prepared statement "q1a_distinct" already exists
Time: 0.715 ms
postgres=# explain analyze execute q1a_distinct;
                                                                                 QUERY PLAN                                                                                 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Aggregate  (cost=30010.06..30010.07 rows=1 width=4) (actual time=9.987..9.987 rows=1 loops=1)
   ->  Nested Loop  (cost=6482.03..30010.06 rows=3 width=4) (actual time=0.314..9.934 rows=142 loops=1)
         Join Filter: (mc.movie_id = t.id)
         ->  Hash Join  (cost=6481.60..30008.29 rows=3 width=8) (actual time=0.302..9.292 rows=142 loops=1)
               Hash Cond: (mc.company_type_id = ct.id)
               ->  Nested Loop  (cost=6462.68..29987.21 rows=566 width=12) (actual time=0.266..9.193 rows=147 loops=1)
                     ->  Nested Loop  (cost=6462.25..22168.36 rows=12213 width=4) (actual time=0.137..0.243 rows=250 loops=1)
                           ->  Seq Scan on info_type it  (cost=0.00..2.41 rows=1 width=4) (actual time=0.031..0.033 rows=1 loops=1)
                                 Filter: ((info)::text = 'top 250 rank'::text)
                                 Rows Removed by Filter: 112
                           ->  Bitmap Heap Scan on movie_info_idx mi_idx  (cost=6462.25..18715.86 rows=345009 width=8) (actual time=0.100..0.160 rows=250 loops=1)
                                 Recheck Cond: (info_type_id = it.id)
                                 Heap Blocks: exact=2
                                 ->  Bitmap Index Scan on info_type_id_movie_info_idx  (cost=0.00..6375.99 rows=345009 width=0) (actual time=0.070..0.070 rows=250 loops=1)
                                       Index Cond: (info_type_id = it.id)
                     ->  Index Scan using movie_id_movie_companies on movie_companies mc  (cost=0.43..0.63 rows=1 width=8) (actual time=0.035..0.035 rows=1 loops=250)
                           Index Cond: (movie_id = mi_idx.movie_id)
                           Filter: ((note !~~ '%(as Metro-Goldwyn-Mayer Pictures)%'::text) AND ((note ~~ '%(co-production)%'::text) OR (note ~~ '%(presents)%'::text)))
                           Rows Removed by Filter: 33
               ->  Hash  (cost=18.88..18.88 rows=4 width=4) (actual time=0.023..0.023 rows=1 loops=1)
                     Buckets: 1024  Batches: 1  Memory Usage: 9kB
                     ->  Seq Scan on company_type ct  (cost=0.00..18.88 rows=4 width=4) (actual time=0.017..0.019 rows=1 loops=1)
                           Filter: ((kind)::text = 'production companies'::text)
                           Rows Removed by Filter: 3
         ->  Index Scan using title_pkey on title t  (cost=0.43..0.58 rows=1 width=8) (actual time=0.004..0.004 rows=1 loops=142)
               Index Cond: (id = mi_idx.movie_id)
 Execution time: 10.158 ms
(27 rows)

Time: 10.551 ms
postgres=# execute q1a_distinct;
 movie_year 
------------
         57
(1 row)

Time: 20.732 ms
postgres=# execute q1a_distinct;
 movie_year 
------------
         57
(1 row)

Time: 18.280 ms
```

The execution plan is a bit bushy so I can't copy it perfectly without caching or factorisation, but I can approximate it with this ordering.

``` julia 
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

# 0.004359 seconds (1.05 k allocations: 35.516 KB)
# 0.002895 seconds (1.05 k allocations: 35.516 KB)
# 0.003321 seconds (1.05 k allocations: 35.516 KB)
```

So my code is fine, I'm just a crappy query planner.

I only had an hour or two to work on this today, but I'm glad I got to see some exciting numbers.

### 2016 Aug 10

So here is q3a:

``` julia

function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title],
  [mi_info::String, mi_id::Int64, k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_production_year::Int64, t_title::String],
  begin 
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info", "info"](mi_id, mi_info)
    (mi_info in mi_infos) == true
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info", "movie_id"](mi_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
  end)
end
```

It touches `movie_info` which is one of the biggest tables in the dataset at ~15m rows. This takes forever to index, so long that I haven't succesfully waited it out yet.

(I have to restart Julia if something gets stuck in a loop or is taking too long, which means reloading the IMDB dataset. Sometimes Atom gets stuck and can't restart Julia, so I have to restart Atom too. Sometimes Atom forgets my project settings, so I have to reopen and reorganize all the files I'm working with. This costs me a significant proportion of the day and the endless context switches are a huge problem. What can I improve?)

But if I index a similarly-sized relation full of random integers:

``` julia 
edge = Relation((rand(1:Int64(1E5), Int64(15E6)), rand(1:Int64(1E5), Int64(15E6))))
@time index(edge, [1,2])
# 2.178177 seconds (146.18 k allocations: 235.030 MB, 0.84% gc time)
```

Even if I put in a string column and force it so touch all the strings:

``` julia
edge = Relation(([1 for _ in 1:Int64(15E6)], String[string(i) for i in rand(1:Int64(1E5), Int64(15E6))]))
@time index(edge, [1,2])
# 17.183060 seconds (59 allocations: 228.885 MB)
```

Sorting the text version of movie_info at the terminal works ok:

``` 
jamie@machine:~$ time sort job/movie_info.csv > /dev/null

real	0m8.972s
user	0m30.316s
sys	0m4.148s
```

So what's the deal? Why does this take seconds when `movie_info` takes hours or more? 

Maybe there's a bug in my quicksort? Let's print the lo/hi for each recursive call and see if it's getting stuck somewhere.

Eugh, atom crashed. Maybe let's print to a file then.

```
...
14431187 14431188
14431184 14431185
14431181 14431182
14431178 14431179
14431175 14431176
14431172 14431173
14431169 14431170
14431166 14431167
14431163 14431164
14431160 14431161
14431157 14431158
14431154 14431155
14431151 14431152
14431148 14431149
14431145 14431146
14431142 14431143
14431139 14431140
14431136 14431137
14431133 14431134
14431130 14431131
14431127 14431128
14431124 14431125
14431121 14431122
14431118 14431119
14431115 14431116
14431112 14431113
...
```

Look at that, recursive calls to `quicksort!` on a bunch of single element subarrays, spaced out by exactly 3 each time. Something funky is going on.

Let's look at the function I copied from the stdlib. There is some weirdness in here where it sorts the smallest partition first and then recurs on the larger partition.

``` julia 
function quicksort!($(cs...), lo::Int, hi::Int)
  write(test, string(lo, " ", hi, "\n"))
  @inbounds while lo < hi
    if hi-lo <= 20 
      insertion_sort!($(cs...), lo, hi)
      return 
    end
    j = partition!($(cs...), lo, hi)
    if j-lo < hi-j
      lo < (j-1) && quicksort!($(cs...), lo, j-1)
      lo = j+1
    else
      j+1 < hi && quicksort!($(cs...), j+1, hi)
      hi = j-1
    end
  end
  return
end
```

It also does something funky to get lo/mid/hi in the right order before partitioning:

``` julia
@inline function select_pivot!($(cs...), lo::Int, hi::Int)
  @inbounds begin
    mi = (lo+hi)>>>1
    if lt($(cs...), mi, lo)
      swap2($(cs...), lo, mi)
    end
    if lt($(cs...), hi, mi)
      if lt($(cs...), hi, lo)
        swap3($(cs...), lo, mi, hi)
      else
        swap2($(cs...), mi, hi)
      end
    end
    swap2($(cs...), lo, mi)
  end
  return lo
end
```

Let's try just picking pivots at random.

``` julia 
function partition!($(cs...), lo::Int, hi::Int)
  @inbounds begin
    pivot = rand(lo:hi)
    swap2($(cs...), pivot, lo)
    i, j = lo+1, hi
    while true
      while lt($(cs...), i, lo); i += 1; end;
      while lt($(cs...), lo, j); j -= 1; end;
      i >= j && break
      swap2($(cs...), i, j)
      i += 1; j -= 1
    end
    swap2($(cs...), lo, j)
    return j
  end
end

function quicksort!($(cs...), lo::Int, hi::Int)
  @inbounds if hi-lo <= 0
    return
  elseif hi-lo <= 20 
    insertion_sort!($(cs...), lo, hi)
  else
    j = partition!($(cs...), lo, hi)
    quicksort!($(cs...), lo, j-1)
    quicksort!($(cs...), j+1, hi)
  end
end
```

Not totally sure that's correct, but I haven't found any mis-sorts so far. 

Sorting becomes slightly slower, maybe around 10%, not enough to make me care, because:

``` julia 
@time index(job["movie_info", "info"], [1,2])
# 1.450726 seconds (210.51 k allocations: 235.458 MB)
```

``` julia
function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_title::String, t_production_year::Int64, mi_id::Int64, mi_info::String],
  begin 
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info", "info"](mi_id, mi_info)
    (mi_info in mi_infos) == true
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info", "movie_id"](mi_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
  end)
end

# Job.q3a x1 (+compilation +indexing)
#  5.088275 seconds (545.92 k allocations: 692.371 MB)
# Job.q3a x20
#  2.178364 seconds (3.60 k allocations: 510.625 KB)
```

```
postgres=# prepare q3a_distinct as SELECT count(distinct t.title) AS movie_title FROM keyword AS k, movie_info AS mi, movie_keyword AS mk, title AS t WHERE k.keyword  like '%sequel%' AND mi.info  IN ('Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German') AND t.production_year > 2005 AND t.id = mi.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi.movie_id AND k.id = mk.keyword_id;
PREPARE
Time: 5.955 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 2596.093 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 220.938 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 187.519 ms
postgres=# execute q3a_distinct;
 movie_title 
-------------
         105
(1 row)

Time: 177.598 ms
```

About 2x faster than postgres on this one. Imp is a bit handicapped atm because it can't turn that `in` into a join, and instead gets stuck with a table scan. Should be an easy optimisation to add though.

I wrote q4a early while waiting on q3a, so let's try that too.

``` julia 
function q4a()
  @query([mii_info],
  [k_keyword::String, k_id::Int64, mk_id::Int64, t_id::Int64, t_production_year::Int64, it_info::String, it_id::Int64, mii_id::Int64, mii_info::String],
  begin
    job["info_type", "info"](it_id, it_info)
    it_info = "rating"
    job["keyword", "keyword"](k_id, k_keyword)
    contains(k_keyword, "sequel") == true
    job["movie_info_idx", "info"](mii_id, mii_info)
    mii_info > "5.0"
    job["title", "production_year"](t_id, t_production_year)
    t_production_year > 2005
    job["movie_info_idx", "movie_id"](mii_id, t_id)
    job["movie_keyword", "movie_id"](mk_id, t_id)
    job["movie_keyword", "keyword_id"](mk_id, k_id)
    job["title", "title"](t_id, t_title)
    job["movie_info_idx", "info_type_id"](mii_id, it_id)
    job["movie_info_idx", "movie_id"](mii_id, t_id)
  end)
end

# Job.q4a x1 (+compilation +indexing)
#   0.552385 seconds (63.57 k allocations: 43.060 MB)
# Job.q4a x100
#   5.834656 seconds (24.30 k allocations: 5.669 MB)
```

```
postgres=# prepare q4a_distinct as SELECT count(distinct mi_idx.info) AS rating, MIN(t.title) AS movie_title FROM info_type AS it, keyword AS k, movie_info_idx AS mi_idx, movie_keyword AS mk, title AS t WHERE it.info ='rating' AND k.keyword  like '%sequel%' AND mi_idx.info  > '5.0' AND t.production_year > 2005 AND t.id = mi_idx.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi_idx.movie_id AND k.id = mk.keyword_id AND it.id = mi_idx.info_type_id;
PREPARE
Time: 0.420 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 226.453 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 123.440 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 119.281 ms
postgres=# execute q4a_distinct;
 rating |                movie_title                 
--------+--------------------------------------------
     45 | 20-seiki shônen: Dai 2 shô - Saigo no kibô
(1 row)

Time: 123.111 ms
```

A little over 2x faster than postgres. 

That's all I have time for today. What next? I could keep going with these benchmarks and setup proper harnesses and tune postgres properly so they are, you know, actual benchmarks and not just a for loop and some guess work. I could fix the query syntax, which is painful and error-prone and would be nice to fix before writing out 100-odd queries. I could add some automated tests instead of hand-checking things against sql.

I have two more days before I go climbing, so maybe I'll see if I can come up with a nicer syntax in that time. Adding more benchmark queries is something that's easy to do with little chunks of time while travelling, but figuring out the syntax requires some concentration.

### 2016 Aug 11

There are two problems I want to fix with the syntax.

First, naming the tables is verbose and error prone eg `job["company_type", "kind"]`. It would be nice to just call this `kind` and somehow resolve the ambiguity with other similary named tables.

Second, the bulk of each the queries so far consists of chains of lookups which are difficult to follow in this form (and in sql too). Compare:

``` sql 
FROM company_type AS ct,
     info_type AS it,
     movie_companies AS mc,
     movie_info_idx AS mi_idx,
     title AS t
WHERE ct.kind = 'production companies'
  AND it.info = 'top 250 rank'
  AND mc.note NOT LIKE '%(as Metro-Goldwyn-Mayer Pictures)%'
  AND (mc.note LIKE '%(co-production)%'
       OR mc.note LIKE '%(presents)%')
  AND ct.id = mc.company_type_id
  AND t.id = mc.movie_id
  AND t.id = mi_idx.movie_id
  AND mc.movie_id = mi_idx.movie_id
  AND it.id = mi_idx.info_type_id;
```

```
title.movie_info.info_type.info = 'top 250 rank'
title.movie_companies.company_type.kind = 'production_companies'
title.movie_companies.note = note
!note.like('%(as Metro-Goldwyn-Mayer Pictures)%') and 
  (note.like('%(co-production)%') || note.like('%(presents)%'))
```

The structure and intent of the query is so much more obvious in the latter (made-up) syntax.

SQL handles the first problem by using tables as namespaces. This has the disadvantage that the namespace is closed - if I want to add more information about, say, `title`, I have to do so with a new table that refers to `title` with a foreign key, and I'm back to the chaining problem.

LogicBlox has a neat syntax where relations are reduced to sixth normal form and treated as finite functions, so one can write:

```
info(info_type(movie_info(title))) = 'top 250 rank'
```

It doesn't have any disambiguation mechanism other than namespaces though, so in practice that might be something like:

```
movei_info.info(movie_info.info_type(title.movie_info(title))) = 'top 250 rank'
```

Datomic (and I think Eve?) has an alternative option - rather than disambiguating several `name` relations, you just emit `7 :name "Best Film Ever"` and ensure that the entity `7` is not used anywhere else. Effectively the disambiguation is done by whatever constructs the entity ids, rather than by the relation name.

The main thing I dislike about this is the existence of unique entity ids. Creating unique identities is easy enough in an imperative model - just generate something random at transaction time. But in a timeless model, identity is much trickier. I don't really have a firm enough grasp on what I mean by that to explain it, but I have a fuzzy sort of intuition that it's an important problem and that existing programming models handle it in a way that causes problems. Certainly, it's a problem I run into in many different areas. I don't have a good idea of what I'm going to do about it, so I don't want to pick a model that railroads me into any particular notion of identity.

So, back to the problem. I could make this nicer with a combination of foreign key declarations and type inference, so that we can have many relations called `name` but each must have a different schema.

```
name(title.id) -> string
name(company.id) -> string

t::title 
t.name # resolves to name(title.id) -> string
```

This is really appealing because it recovers the SQL-style namespacing, but allows open additions, doesn't require ids to be globally unique and can handle multi-column keys. The use of foreign key constraints in the schema also allows for auto-completing such lookups in the future.

A year or two ago I would probably have jumped in and started working on this. These days I'm a bit warier.

This system requires a database schema, which is known at compile time. I have to write a type-inference algorithm. There needs to be some way to report ambiguous names to the user. It only works for foreign-key joins, so there needs to be some separate system for disambiguating other joins. It's not obviously open to extension. And all I get for that effort is slightly less typing. 

A mistake I used to make far too often is to make design decisions like these based only on how the value of the outcome, rather than on the effort-value ratio.

Let's do something much simpler. We can clean up the chaining by switching the syntax from `relation(x,y,z)` to `(x,y,z) > relation`, and allowing prefixes such as `(x,y) > relation = z` and `x.relation > (y,z)`, and similarly `(x,y,z) < relation`, `(y,z) < relation = x`, `z < relation = (x,y)`. This allows writing chains like `title < movie_info_title > movie_info_type < info_type_info = 'top 250 rank'` in most cases, but without losing the full generality of relations.

We'll still allow arbitrary Julia expressions as relation names,. So depending on how the relations are stored in Julia we could write any one of:

``` 
title < movie_info_title > movie_info_type < info_type_info = "top 250 rank"
title < Job.movie_info.title > Job.movie_info.type < Job.info_type.info = "top 250 rank"
title < job["movie_info", "title"] > job["movie_info", "type"] < job["info_type", "info"] = "top 250 rank"
```

i.e. rather than writing our own namespace system for Imp, just call out to Julia and let the user do whatever.

We also need a way to execute Julia functions and filters. Let's use `=` to signify that the RHS is arbitrary Julia code to be run on each tuple, rather than a relational Imp expression:

```
x::Int64 = y + 1 # assignment
true = x > y # filter
```

This is becoming a theme in Imp - handling only the core value-adding parts myself and fobbing everything else off on Julia. It's very similar to how [Terra](http://terralang.org/) handles low-level coding but delegates namespaces, packaging, macros, polymorphism etc to Lua. 


(We could maybe even add a macro system to allow eg:

```
@path title info
```

Where `path` is some user-defined function that reads a schema, figures out the obvious join path between title and info, and returns the corresponding query fragment. [This paper](http://homepages.inf.ed.ac.uk/wadler/papers/qdsl/pepm.pdf) has some really interesting ideas along those lines.)

Let's write out the first few JOB queries in this imagined syntax, to see how it behaves:

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" < info_type.info < movie_info.info_type < movie_info
  movie_info > movie_info.movie_id > title
  title > title.production_year > production_year
  title < movie_companies.movie_id < movie_company
  movie_company > movie_companies.company_type > company_type.kind > "production companies"
  movie_company > movie_companies.note > note 
  true = !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents")
end
```

Hmmm. It's *ok*. Syntax highlighting to separate variables from relations would really help.

You might notice that I don't really need `<`, since eg `title < movie_companies.movie_id < movie_company` could be written as `movie_company > movie_companies.movie_id > title`, and using both in the same line is actually kind of confusing. But... I want to have the flexibility to use both because I want to convey variable ordering directly in the query, by just taking the first occurence of each variable. Eg the above query would produce the ordering `[#info, #info_type, movie_info, title, production_year, movie_company, #company_type, #kind, #note]` (where variables beginning with # are unnamed intermediates in chains).

The `true = ...` is gross though. Maybe I should pick a symbol that's less commonly used in Julia, like `|>` or `>>`, and declare that any line containing that symbol is an Imp line. I wish I could use `->` and `<-` but Julia doesn't parse those as functions calls.

``` julia 
julia> :(foo -> bar -> baz)
:(foo->begin  # none, line 1:
            bar->begin  # none, line 1:
                    baz
                end
        end)

julia> :(foo <- bar <- baz)
:(foo < -bar < -baz)
```

Hmmm, let's see:

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" << info_type.info << movie_info.info_type << movie_info
  movie_info >> movie_info.movie_id >> title
  title >> title.production_year >> production_year
  title << movie_companies.movie_id << movie_company
  movie_company >> movie_companies.company_type >> company_type.kind >> "production companies"
  movie_company >> movie_companies.note >> note 
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

``` julia
q1a = @query(production_year) begin 
  "top 250 rank" <| info_type.info <| movie_info.info_type <| movie_info
  movie_info |> movie_info.movie_id |> title
  title |> title.production_year |> production_year
  title <| movie_companies.movie_id <| movie_company
  movie_company |> movie_companies.company_type |> company_type.kind |> "production companies"
  movie_company |> movie_companies.note |> note 
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

I find the latter a little more readable. Let's go with that. `|>` is also used for function chaining in the Julia stdlib, so it's a nice analogy.

Let's check the other queries:

``` julia
q2a = @query(name) begin
  "character-name-in-title" <| keyword.keyword <| movie_keyword.keyword_id <| movie_keyword.movie_id |> title 
  title |> title.name |> name 
  title <| movie_companies.movie_id |> movie_companies.company_id |> company_name.country_code |> "[de]"
end

infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
q3a = @query(name) begin 
  contains(keyword, "sequel")
  keyword <| keyword.keyword <| movie_keyword.keyword_id |> movie_keyword.movie_id |> title 
  title |> title.name |> name 
  title |> title.production_year |> production_year
  production_year > 2005 
  title <| movie_info.movie_id |> movie_info.info |> info 
  info in infos
end

q4a = @query(info) begin 
  contains(keyword, "sequel")
  keyword <| keyword.keyword <| movie_keyword.keyword_id |> move_keyword.movie_id |> title 
  title |> title.production_year |> production_year
  production_year > 2005 
  "rating" <| info_type.info <| movie_info.info_type_id <| movie_info 
  movie_info |> move_info.info |> info 
  info > "5.0"
end
```

Hmmm. It's fine for these bidirectional edges, but it doesn't really work for single column relations eg `vertex(v)`.

Here's a different idea. `keyword <| keyword.keyword <| movie_keyword.keyword_id |> move_keyword.movie_id |> title` could be written as `keyword.keyword(keyword, t1); movie_keyword.keyword_id(t1, t2);  movie_keyword.movie_id(t2, title)` in a more traditional syntax. There's the risk of accidentally reusing a temporary variable, but maybe I could make them line- or block- scoped. 

``` julia
q1a = @query(production_year) begin 
  info_type.info(t1, "top 250 rank"); movie_info.info_type(movie_info, t1); 
  movie_info.movie_id(movie_info, title)
  title.production_year(title, production_year)
  movie_companies.movie_id(movie_company, title)
  movie_companies.company_type(movie_company, t1); company_type.kind(t1, "production companies") 
  movie_companies.note(movie_company, note)
  !contains(note, "as Metro-Goldwyn-Mayer Pictures") && (contains(note, "co-production") || contains(note, "presents"))
end
```

Weirdly, I don't find that as readable. The former had this nice visual emphasis on the variables and the connections between them that this lacks. This one also messes with the variable ordering a little (t1 comes before "top 250 rank"), but that will also happen in the other syntax with >2 columns.

Part of the problem in any case is that the JOB schema is pretty distorted to avoid multiple allocations of the same string, but since we're running in-memory we can just share pointers. With a nicer schema:

``` julia
q4a = @query(rating) begin 
  true = contains(keyword, "sequel")
  movie_keyword(movie, keyword)
  movie_production_year(movie, production_year)
  true = production_year > 2005 
  movie_info(movie, "rating", rating)
  true = rating > "5.0"
end
```

Which is totally readable. 

But what about my variable ordering? Picking the first occurence works ok here, but is that flexible enough in general? Maybe I'll allow adding hints inline if I find a need.

So, actually, all I really need to change is to allow inline constants (which I'll lift to the top of the query) and derive the variable ordering from the query. And do something prettier with aggregates.

### 2016 Aug 14

Some quick little ergonomic improvements tonight. 

I moved type annotations from the variable ordering to the return statement, which is the only place they are now needed and also doubles up as a schema for views. This also simplifed the code for `plan_query` to:

``` julia 
function plan_query(returned_typed_variables, aggregate, variables, query)
  join = plan_join(returned_typed_variables, aggregate, variables, query)

  project_variables = map(get_variable_symbol, returned_typed_variables)
  push!(project_variables, :prev_aggregate)
  project_aggregate = [aggregate[1], aggregate[2], :(prev_aggregate::$(get_variable_type(aggregate[3])))]  
  project_query = quote 
    intermediate($(project_variables...))
  end
  project = plan_join(returned_typed_variables, project_aggregate, project_variables, project_query)
  
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $project
    end
  end
end
```

 I added some code to the compiler that allows writing Julia constants or expressions where Imp variables should be. 
 
 ``` julia 
 for clause in relation_clauses 
   line = query.args[clause]
   for (ix, arg) in enumerate(line.args)
     if ix > 1 && !isa(arg, Symbol)
       variable = gensym("variable")
       line.args[ix] = variable
       assignment_clauses[variable] = arg 
       callable_at = 1 + maximum(push!(indexin(collect_variables(arg), variables), 0))
       insert!(variables, 1, variable)
     end
   end
 end
 ```

I created nicer names for the various JOB tables.

``` julia 
for (table_name, column_name) in keys(job)
  @eval begin 
    $(symbol(table_name, "_", column_name)) = job[$table_name, $column_name]
    export $(symbol(table_name, "_", column_name))
  end
end
```

I rewrote each job query so that the order in which in each variable first appears matches the variable ordering I chose, and then changed `plan_query` to use this ordering directly. It also allows simply mentioning a variable to insert in the order. 

``` julia
variables = []
for clause in 1:length(query.args)
  line = query.args[clause]
  if clause in hint_clauses 
    push!(variables, line)
  elseif clause in relation_clauses
    for (ix, arg) in enumerate(line.args)
      if ix > 1 && !isa(arg, Symbol)
        variable = gensym("variable")
        line.args[ix] = variable
        assignment_clauses[variable] = arg 
        insert!(variables, 1, variable) # only handles constants atm
      elseif ix > 1 && isa(arg, Symbol)
        push!(variables, arg)
      end
    end
  end
end
variables = unique(variables)
```

It doesn't look inside assignments or expressions yet, but I just use hints to work around that for now.

The job queries now look like:

``` julia
function q1a()
  @query([t_production_year::Int64],
  begin 
    info_type_info(it_id, "top 250 rank")
    movie_info_idx_info_type_id(mii_id, it_id)
    movie_info_idx_movie_id(mii_id, t_id)
    movie_companies_movie_id(mc_id, t_id)
    movie_companies_company_type_id(mc_id, ct_id)
    company_type_kind(ct_id, "production companies")
    movie_companies_note(mc_id, mc_note)
    @when !contains(mc_note, "as Metro-Goldwyn-Mayer Pictures") &&
      (contains(mc_note, "co-production") || contains(mc_note, "presents"))
    title_production_year(t_id, t_production_year)
  end)
end

function q2a()
  @query([title::String],
  begin
    keyword_keyword(k_id, "character-name-in-title")
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    movie_companies_movie_id(mc_id, t_id)
    movie_companies_company_id(mc_id, cn_id)
    company_name_country_code(cn_id, "[de]") 
    title_title(t_id, title)
  end)
end

function q3a()
  # "Denish" is in original too
  mi_infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query([t_title::String],
  begin 
    k_keyword
    @when contains(k_keyword, "sequel")
    keyword_keyword(k_id, k_keyword)
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    title_title(t_id, t_title)
    title_production_year(t_id, t_production_year)
    @when t_production_year > 2005
    movie_info_movie_id(mi_id, t_id)
    movie_info_info(mi_id, mi_info)
    @when mi_info in mi_infos
  end)
end

function q4a()
  @query([mii_info::String],
  begin
    k_keyword
    @when contains(k_keyword, "sequel")
    keyword_keyword(k_id, k_keyword)
    movie_keyword_keyword_id(mk_id, k_id)
    movie_keyword_movie_id(mk_id, t_id)
    title_production_year(t_id, t_production_year)
    @when t_production_year > 2005
    info_type_info(it_id, "rating")
    movie_info_idx_info_type_id(mii_id, it_id)
    movie_info_idx_movie_id(mii_id, t_id)
    movie_info_idx_info(mii_id, mii_info)
    @when mii_info > "5.0"
  end)
end
```

The remaining grossness is mostly just the awful table/variable names from the original benchmark. I'm ok with that. 

I'm going on a long climbing trip, so the next few weeks will be sparse.

### 2016 Aug 20

Fixed a sorting bug - choosing the pivot at random breaks the invariant that there is always at least one element smaller or larger than the pivot, so the partitioning can run off the end of the array.

``` 
diff --git a/src/Data.jl b/src/Data.jl
index bccfa6f..1088331 100644
--- a/src/Data.jl
+++ b/src/Data.jl
@@ -60,8 +60,8 @@ function define_columns(n)
         swap2($(cs...), pivot, lo)
         i, j = lo+1, hi
         while true
-          while lt($(cs...), i, lo); i += 1; end;
-          while lt($(cs...), lo, j); j -= 1; end;
+          while (i <= j) && lt($(cs...), i, lo); i += 1; end;
+          while (i <= j) && lt($(cs...), lo, j); j -= 1; end;
           i >= j && break
           swap2($(cs...), i, j)
           i += 1; j -= 1
```

### 2016 Aug 29

I added support for `in` so that we can write things like:

``` julia 
@query([x,y],
begin
  x in 1:10
  y in 1:10
  @when x < y 
end)
```

It works almost identically to `=`, except that it loops over the result of the expression instead of assigning.

``` julia 
body = quote 
  for $(esc(variable)) in $(esc(loop_clauses[variable]))
    if assign($variable_columns, los, ats, his, $variable_ixes, $(esc(variable)))
      $body
    end
  end
end
```

I could also have sorted the result and treated it like another column for intersection, but that would have been a bit more work and I'm not sure yet whether it would pay off.

I also removed a limitation of the variable ordering detection where it only looked at grounded variables. It can now look inside `=`, `in` and `@when`.

### 2016 Aug 30

Going to start looking at UI. I'll need to do more work on queries and dataflow along the way, but I think it will be helpful to do add features as they are required by real programs, rather than planning them in advance and finding later that they aren't quite right. 

I'm going with HTML just because it's familiar and widely supported. 

With Blink.jl and Hiccup.jl it's really easy to get a window up and display content:

``` julia 
w = Window()
body!(w, Hiccup.div("#foo.bar", "Hello World"))
```

Handling events is a bit harder. There is a [issue thread](https://github.com/JunoLab/Blink.jl/issues/57) but I'm just reproducing the same error as the person asking the question. To the debugger! Which I haven't used before...

``` julia 
using Gallium
breakpoint(Blink.ws_handler)
```

```
signal (11): Segmentation fault
while loading /home/jamie/.atom/packages/julia-client/script/boot.jl, in expression starting on line 327
evaluate_generic_instruction at /home/jamie/.julia/v0.5/DWARF/src/DWARF.jl:376
unknown function (ip: 0x7f9323b5a5c9)
...
```

Bah. To be fair, I have a pretty janky setup with various packages running at weird versions on top of a Julia RC. When Julia 0.5 is released I'll clean it up and try the debugger again.

Instead I just poke around in the source code and eventually figure out that the data sent back has to be a dict, and that there is a baked-in magic function in `@js` for making such. 

``` julia 
x = [1,2]
@js_ w document.getElementById("my_button").onclick = () -> Blink.msg("press", d(("foo", $x)))
handle(w, "press") do args...
  @show args
end
```

But I want these callbacks to be specified by values in the dom, not by a separate side-effect. 

``` julia
function event(table_name, values)
  Blink.jsexpr(quote 
    Blink.msg("event", d(("table", $table_name), ("values", $values)))
  end).s
end

macro event(expr)
  assert(expr.head == :call)
  :(event($(string(expr.args[1])), [$(expr.args[2:end]...)]))
end

function Window(event_tables)
  w = Window()
  event_number = 1
  handle(w, "event") do args 
    values = args["values"]
    insert!(values, 1, event_number)
    event_number += 1
    push!(event_tables[args["table"]], values)
  end
end

macro Window(event_tables...)
  :(Window(Dict($([:($(string(table)) => $table) for table in event_tables]...))))
end
```

``` julia
using Data
clicked = Relation((Int64[], String[]))
w = @Window(clicked)
body!(w, button("#my_button", Dict(:onclick => @event clicked("my_button")), "click me!"))
```

I haven't actually implemented `push!` yet for relations, so let's do that too. I'm still just using sorted arrays so this is a little hacky. It'll do for now.

``` julia 
function Base.push!{T}(relation::Relation{T}, values)
  assert(length(relation.columns) == length(values))
  for ix in 1:length(values)
    push!(relation.columns[ix], values[ix])
  end
  empty!(relation.indexes)
  # TODO can preserve indexes when inserted value is at end or beginning
  # TODO remove dupes
end
```

Uh, but I don't have a proper dataflow yet and I'll want to run things on each event, so maybe this is poorly thought out. Let's add a callback to the window:

``` julia 
function Blink.Window(flow, event_tables)
  w = Window()
  event_number = 1
  handle(w, "event") do args 
    values = args["values"]
    insert!(values, 1, event_number)
    push!(event_tables[args["table"]], values)
    flow(w, event_number)
    event_number += 1
  end
  flow(w, 0)
  w
end

macro Window(flow, event_tables...)
  :(Window($flow, Dict($([:($(string(table)) => $table) for table in event_tables]...))))
end
```

``` julia 
clicked = Relation((Int64[], String[]))
@Window(clicked) do w, event_number
  body!(w, button("#my_button", Dict(:onclick => @event clicked("my_button")), "clicked $event_number times"))
end
```

Somehow I ended up tidying up code and setting up proper tests. There doesn't seem to be much builtin structure for tests so I just have a scratch file to run things from:

``` julia 
include("src/Data.jl")
include("src/Query.jl")
include("src/UI.jl")

include("examples/JobData.jl")

include("examples/Graph.jl")
include("examples/Chinook.jl")
include("examples/Job.jl")

Graph.test()
Chinook.test()
Job.test()

Graph.bench()
Chinook.bench()
Job.bench()
```

I need some examples to work with to figure out what to implement next. I started with a simple minesweeper game. I don't think it's a particularly good usecase for Imp, but someone recently posted an Eve version and I was feeling cheeky. A sketch of the core mechanics:

``` julia
function run(num_x, num_y, num_mines)
  @relation state() => Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) => Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) => Int64, Int64
  
  @query begin 
    + state() = :game_ok
  end
  
  while length(mine) < num_mines
    @query begin 
      x = rand(1:num_x)
      y = rand(1:num_y)
      + mine(x,y)
    end 
  end
  
  @query begin 
    x in 1:num_x
    y in 1:num_y
    c = count(
      nx in -1:1
      ny in -1:1
      @when (nx != 0) || (ny != 0)
      mine(x+nx, y+ny) = true
    )
    + mine_count(x, y) = c
  end
  
  @Window(clicked) do display, event_number
    @query begin
      clicked($event_number) = (x, y)
      + cleared(x, y)
    end
    
    fix!(cleared) do
      @query begin 
        cleared(x,y)
        mine_count(x,y,0)
        nx in -1:1
        ny in -1:1
        @when (nx * ny == 0) && (nx + ny != 0) # no boolean xor :(
        + cleared(x+nx, y+ny)
      end)
    end
    
    @query 
      clicked($event_number) = (x, y)
      mine(x,y)
      + state() = :game_over
    end
    
    @query begin 
      state() = state
      x in 1:num_x
      y in 1:num_y
      cleared = exists(cleared(x,y))
      mine = exists(mine(x,y))
      mine_count(x,y,count)
      node = @match (state, mine, cleared, count) begin
        (:game_over, true, _, _) => button("💣")
        (:game_over, false, _, _) => button(string(count))
        (:game_ok, _, true, 0) => button(" ")
        (:game_ok, _, true, _) => button(string(count))
        (:game_ok, _, false, _) => button("X", :onclick => @event clicked(x,y))
      end
      @group y node = h_box(node)
      @group x node = v_box(node)
      + display() = node
    end
    
  end
end
```

This requires:

* a relation macro that records a functional dependecy 
* query syntax updated to match 
* syntax for upsert into a relation 
* (probably also want delete)
* with change tracking to handle fix!
* better aggregates / subqueries / negation 

The last point is a design problem that has been bugging me for ages, so it bears some thinking about. 

Fundeps / upsert is simpler, but it does move Imp away from being a general purpose library. It probably won't be hard to support a separate macro that just returns results though.

I was imagining that eg `+ mine_count(x, y) = c` would replace any existing value for `(x, y)`, but what should happen if a single query execution produces multiple values of `c` for a single `(x,y)`. Probably an error?

Well, let's start with something I do know how to implement:

``` julia 
type Relation{T <: Tuple} # where T is a tuple of columns
  columns::T
  indexes::Dict{Vector{Int64},T}
  key_types::Vector{Type}
  val_types::Vector{Type}
end

# examples:
# @relation height_at(Int64, Int64) = Float64
# @relation married(String, String)
# @relation state() = (Int64, Symbol)
macro relation(expr) 
  if expr.head == :(=)
    name_and_keys = expr.args[1]
    vals_expr = expr.args[2]
  else 
    name_and_keys = expr
    vals_expr = Expr(:tuple)
  end
  assert(name_and_keys.head == :call)
  name = name_and_keys.args[1]
  assert(isa(name, Symbol))
  keys = name_and_keys.args[2:end]
  for key in keys 
    assert(isa(key, Symbol))
  end
  if vals_expr.head == :block
    vals_expr = vals_expr.args[2]
  end
  if isa(vals_expr, Symbol) 
    vals = [vals_expr]
  else 
    assert(vals_expr.head == :tuple)
    vals = vals_expr.args
  end
  for val in vals 
    assert(isa(val, Symbol))
  end
  typs = [keys..., vals...]
  quote 
    columns = tuple($([:(Vector{$typ}()) for typ in typs]...))
    indexes = Dict{Vector{Int64}, typeof(columns)}()
    $(esc(name)) = Relation(columns, indexes, Type[$(keys...)], Type[$(vals...)])
  end
end
```

### 2016 Sep 1

Next thing I need is a way to merge relations, with the values from the more recent version winning key collisions. I also threw in a function that checks the fundep invariant. 

``` julia 
function define_keys(n, num_keys)
  olds = [symbol("old", c) for c in 1:n]
  news = [symbol("new", c) for c in 1:n]
  results = [symbol("result", c) for c in 1:n]
  ts = [symbol("C", c) for c in 1:n]
  
  quote 
  
    function merge_sorted!{$(ts...)}(old::Tuple{$(ts...)}, new::Tuple{$(ts...)}, result::Tuple{$(ts...)}, num_keys::Type{Val{$num_keys}})
      @inbounds begin
        $([:($(olds[c]) = old[$c]) for c in 1:n]...)
        $([:($(news[c]) = new[$c]) for c in 1:n]...)
        $([:($(results[c]) = result[$c]) for c in 1:n]...)
        old_at = 1
        new_at = 1
        old_hi = length($(olds[1]))
        new_hi = length($(news[1]))
        while old_at <= old_hi && new_at <= new_hi
          c = c_cmp($(olds[1:num_keys]...), $(news[1:num_keys]...), old_at, new_at)
          if c == 0
            $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
            old_at += 1
            new_at += 1
          elseif c == 1
            $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
            new_at += 1
          else 
            $([:(push!($(results[c]), $(olds[c])[old_at])) for c in 1:n]...)
            old_at += 1
          end
        end
        while old_at <= old_hi
          $([:(push!($(results[c]), $(olds[c])[old_at])) for c in 1:n]...)
          old_at += 1
        end
        while new_at <= new_hi
          $([:(push!($(results[c]), $(news[c])[new_at])) for c in 1:n]...)
          new_at += 1
        end
      end
    end
    
    function assert_no_dupes_sorted{$(ts...)}(result::Tuple{$(ts...)}, num_keys::Type{Val{$num_keys}})
      $([:($(results[c]) = result[$c]) for c in 1:n]...)
      for at in 2:length($(results[1]))
        assert(c_cmp($(results[1:num_keys]...), $(results[1:num_keys]...), at, at-1) == 1)
      end
    end
    
  end
end

for n in 1:10
  for k in 1:n
    eval(define_keys(n, k))
  end
end

function Base.merge{T}(old::Relation{T}, new::Relation{T})
  # TODO should Relation{T} be typed Relation{K,V} instead?
  assert(old.key_types == new.key_types)
  assert(old.val_types == new.val_types)
  result_columns = tuple([Vector{eltype(column)}() for column in old.columns]...)
  order = collect(1:(length(old.key_types) + length(old.val_types)))
  merge_sorted!(index(old, order), index(new, order), result_columns, Val{length(old.key_types)})
  result_indexes = Dict{Vector{Int64}, typeof(result_columns)}(order => result_columns)
  Relation(result_columns, result_indexes, old.key_types, old.val_types)
end

function assert_no_dupes{T}(relation::Relation{T})
  order = collect(1:(length(relation.key_types) + length(relation.val_types)))
  assert_no_dupes_sorted(index(relation, order), Val{length(relation.key_types)})
  relation
end
```

There's all kinds of grossness in here, similar to the sort functions before, dealing with the annoying restrictions on stack allocation. Might be worth cleaning this up before I continue.

First, let's add some microbenchmarks to make sure I don't screw anything up.

``` julia 
function bench()
  srand(999)
  x = rand(Int64, 10000)
  @show @benchmark quicksort!((copy($x),))
  
  srand(999)
  y = [string(i) for i in rand(Int64, 10000)]
  @show @benchmark quicksort!((copy($y),))
  
  srand(999)
  x = unique(rand(1:10000, 10000))
  y = rand(1:10000, length(x))
  z = rand(1:10000, length(x))
  a = Relation((x,y), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
  b = Relation((x,z), Dict{Vector{Int64}, typeof((x,y))}(), Type[Int64], Type[Int64])
  @show @benchmark merge($a,$b)
end
```

``` julia
@benchmark(quicksort!((copy($(Expr(:$, :x))),))) = BenchmarkTools.Trial: 
  samples:          8320
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  78.22 kb
  allocs estimate:  3
  minimum time:     463.05 μs (0.00% GC)
  median time:      545.95 μs (0.00% GC)
  mean time:        599.23 μs (4.81% GC)
  maximum time:     11.10 ms (92.95% GC)
@benchmark(quicksort!((copy($(Expr(:$, :y))),))) = BenchmarkTools.Trial: 
  samples:          1025
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  78.22 kb
  allocs estimate:  3
  minimum time:     3.73 ms (0.00% GC)
  median time:      4.72 ms (0.00% GC)
  mean time:        4.87 ms (0.48% GC)
  maximum time:     14.11 ms (58.82% GC)
@benchmark(merge($(Expr(:$, :a)),$(Expr(:$, :b)))) = BenchmarkTools.Trial: 
  samples:          10000
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  258.72 kb
  allocs estimate:  47
  minimum time:     105.06 μs (0.00% GC)
  median time:      115.67 μs (0.00% GC)
  mean time:        196.96 μs (39.01% GC)
  maximum time:     7.80 ms (97.17% GC)
```

The only functions that actually pull a row onto the stack are `cmp`, `lt`, `lt2`, `swap` and `insertion_sort!`. First let's rewrite insertion sort to use swapping, and see if doubling the number of writes slows things down appreciably. 

``` julia
function insertion_sort!($(cs...), lo::Int, hi::Int)
  @inbounds for i = lo+1:hi
    j = i
    while j > lo && lt($(cs...), j, j-1)
      swap($(cs...), j, j-1)
      j -= 1
    end
  end
end
```

Any change to the benchmarks is within the range of noise. `lt2` was only used in `insertion_sort!`, so that leaves us with just `cmp`, `lt` and `swap`. `lt` is redundant.

``` julia
@inline function c_cmp($(olds...), $(news...), old_at, new_at) 
  @inbounds begin 
    $([quote 
      c = cmp($(olds[c])[old_at], $(news[c])[new_at])
      if c != 0; return c; end
    end for c in 1:(n-1)]...)
    return cmp($(olds[n])[old_at], $(news[n])[new_at])
  end
end
 
@inline function swap($(cs...), i, j)
  @inbounds begin
    $([quote
      $(tmps[c]) = $(cs[c])[j]
      $(cs[c])[j] = $(cs[c])[i]
      $(cs[c])[i] = $(tmps[c])
    end for c in 1:n]...)
  end
end
```

Both of these need the loops to be unrolled because the type of `tmp` changes on each iteration. Without unrolling, it will get the type `Any` which will cause it to heap-allocate eg integers that were allocated as values in the array.

``` julia 
@generated function cmp_in{T <: Tuple}(xs::T, ys::T, x_at::Int64, y_at::Int64)
  n = length(T.parameters)
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      $([:(result = cmp(xs[$c][x_at], ys[$c][y_at]); if result != 0; return result; end) for c in 1:(n-1)]...)
      return cmp(xs[$n][x_at], ys[$n][y_at])
    end
  end
end

@generated function swap_in{T <: Tuple}(xs::T, i::Int65, j::Int64)
  n = length(T.parameters)
  quote
    $(Expr(:meta, :inline))
    @inbounds begin 
      $([quote 
        let tmp = xs[$c][i]
          xs[$c][i] = xs[$c][j]
          xs[$c][j] = tmp
        end
      end for c in 1:n]...)
    end
  end
end
```

I'm no longer unpacking the tuple of columns, so I can use `@generated` to generate them on the fly rather than `for n in 1:10; eval(define_columns(n)); end`. 

Now I can make the rest of the sorting code into normal functions:

``` julia 
function insertion_sort!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds for i = lo+1:hi
    j = i
    while j > lo && (cmp_in(cs, cs, j, j-1) == -1)
      swap_in(cs, j, j-1)
      j -= 1
    end
  end
end

function partition!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds begin
    pivot = rand(lo:hi)
    swap_in(cs, pivot, lo)
    i, j = lo+1, hi
    while true
      while (i <= j) && (cmp_in(cs, cs, i, lo) == -1); i += 1; end;
      while (i <= j) && (cmp_in(cs, cs, lo, j) == -1); j -= 1; end;
      i >= j && break
      swap_in(cs, i, j)
      i += 1; j -= 1
    end
    swap_in(cs, lo, j)
    return j
  end
end

function quicksort!{T <: Tuple}(cs::T, lo::Int, hi::Int)
  @inbounds if hi-lo <= 0
    return
  elseif hi-lo <= 20 
    insertion_sort!(cs, lo, hi)
  else
    j = partition!(cs, lo, hi)
    quicksort!(cs, lo, j-1)
    quicksort!(cs, j+1, hi)
  end
end

function quicksort!{T <: Tuple}(cs::T)
  quicksort!(cs, 1, length(cs[1]))
end
```

`merge` and `assert_no_dupes` change similarly.

This adds a bunch of tuple accesses to the hot path, so let's check if it hurt the benchmarks:

``` julia 
@benchmark(quicksort!((copy($(Expr(:$, :x))),))) = BenchmarkTools.Trial: 
  samples:          8569
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  78.22 kb
  allocs estimate:  3
  minimum time:     491.56 μs (0.00% GC)
  median time:      556.94 μs (0.00% GC)
  mean time:        582.50 μs (3.39% GC)
  maximum time:     7.47 ms (91.25% GC)
@benchmark(quicksort!((copy($(Expr(:$, :y))),))) = BenchmarkTools.Trial: 
  samples:          1335
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  78.22 kb
  allocs estimate:  3
  minimum time:     3.26 ms (0.00% GC)
  median time:      3.68 ms (0.00% GC)
  mean time:        3.74 ms (0.47% GC)
  maximum time:     10.67 ms (58.69% GC)
@benchmark(merge($(Expr(:$, :a)),$(Expr(:$, :b)))) = BenchmarkTools.Trial: 
  samples:          7918
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  442.06 kb
  allocs estimate:  11769
  minimum time:     351.98 μs (0.00% GC)
  median time:      398.31 μs (0.00% GC)
  mean time:        630.96 μs (34.79% GC)
  maximum time:     13.66 ms (93.77% GC)
```

Ouch, `merge` got a lot slower and is making a ton of allocations. What went wrong in there?

Oh, that's disappointing. `merge_sorted!` contains:

``` julia
old_key = old[1:num_keys]
new_key = new[1:num_keys]
```

And julia doesn't infer the correct types. Easily fixed though - I'll just pass them as args instead of `num_keys`.

``` julia
@benchmark(merge($(Expr(:$, :a)),$(Expr(:$, :b)))) = BenchmarkTools.Trial: 
  samples:          10000
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  259.06 kb
  allocs estimate:  57
  minimum time:     93.48 μs (0.00% GC)
  median time:      123.75 μs (0.00% GC)
  mean time:        206.21 μs (38.39% GC)
  maximum time:     8.54 ms (96.65% GC)
```

Eh, that'll do.

### 2016 Sep 2

Now I have to deal with aggregates. In terms of expressivity, what I really want are first-class relations within the query language. But I don't want to actually materialize or allow them to end up in the output, because that just brings back all of the pointer-chasing problems of high-level languages. 

One option would be something like [T-LINQ](http://homepages.inf.ed.ac.uk/jcheney/publications/cheney13icfp.pdf) which allows first-class sets in queries but guarantees to normalize them away before execution. I like the principle, but the small addendum in the paper about not being able to normalize aggregates makes it seem like more work is necessary.

What I'm going to do instead is to make queries return an iterator instead of executing all in one go. Then aggregates can be handled by normal Julia functions. This will make queries much harder to analyse when I come to look at things like incremental evaluation, but in the meantime it's easier and safer to implement. I'll come back to T-LINQy ideas later.

Sidenote: I just noticed that Julia has a neat feature that I really wanted when working on Strucjure - you can embed pointers directly into an ast:

``` julia 
xs = [1,2,3]

q = let ys = xs
  :(push!($ys, 4))
end

@show q
# :(push!([1,2,3],4))

@show xs
# xs = [1,2,3]

@show ys
# UndefVarError: ys not defined

eval(q)

@show xs
# xs = [1,2,3,4]

@show q
# :(push!([1,2,3,4],4))
```

So, let's pick an example query:

``` julia
@query begin
  cleared(x,y)
  mine_count(x,y) = 0
  nx in -1:1
  ny in -1:1
  @when (nx * ny == 0) && (nx + ny != 0) # no boolean xor :(
end
```

This is going to be replaced by a couple of closures that close over all the necessary data:

``` julia
begin
  ixes_x = [1,1]
  function start()
    los = [...]
    ats = [...]
    his = [...]
    index_1 = index(cleared, [1,2])
    index_2 = index(mine_count, [3,1,2])
    ...
    row = Row(ats)
    (row, los, ats, his, index_1, index_2, ...)
  end
  function next(state)
    row, los, ats, his, index_1, index_2, ... = state
    ...
    row
  end
  Query(start, next)
end
```

I've already checked in previous experiments that in Julia 0.5 closures can be specialized on, so if the query has a known type then these closures shouldn't cause any dispatch overhead while looping over the results.

### 2016 Sep 5

I let that idea stew and worked on some other projects for a few days. I kept going back and forth between various different implementations with different trade-offs and eventually realized that I was failing to make decision because I don't have enough information. I don't have nearly enough example code and datasets to think about the impact of different implementations.

Let's instead just try to pick something that is easy and doesn't limit future choices too much. What if I had queries just return everything and then used normal Julia functions for aggregation? I can replace the internal aggregation code with the simple optimization that bails out after finding a single solution for each returned row.

``` julia
while ...
  a = ...
  while ...
    b = ...
    need_more_results = true 
    while need_more_results && ...
      c = ...
      while need_more_results && ...
        d = ...
        push!(results, (a, b))
        need_more_results = false
      end
    end
  end
end
```

While working on this I spent hours tracking down a subtle bug. `assign` does not bail out if the value is not found but a higher value is. This doesn't cause any test failures because the later aggregate handling multiplies the aggregate value by number of matching rows in each table. If the value is missing the number of matching rows is 0, so the aggregate is 0 and is not returned. Fixing this yields some mild speedups:

``` julia
@benchmark(q1a()) = BenchmarkTools.Trial: 
  samples:          2932
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  23.20 kb
  allocs estimate:  156
  minimum time:     1.27 ms (0.00% GC)
  median time:      1.68 ms (0.00% GC)
  mean time:        1.70 ms (0.92% GC)
  maximum time:     17.64 ms (86.49% GC)
@benchmark(q2a()) = BenchmarkTools.Trial: 
  samples:          79
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  713.50 kb
  allocs estimate:  249
  minimum time:     61.60 ms (0.00% GC)
  median time:      63.79 ms (0.00% GC)
  mean time:        64.01 ms (0.36% GC)
  maximum time:     71.39 ms (13.57% GC)
@benchmark(q3a()) = BenchmarkTools.Trial: 
  samples:          42
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  30.20 kb
  allocs estimate:  234
  minimum time:     117.45 ms (0.00% GC)
  median time:      119.76 ms (0.00% GC)
  mean time:        119.83 ms (0.00% GC)
  maximum time:     122.71 ms (0.00% GC)
@benchmark(q4a()) = BenchmarkTools.Trial: 
  samples:          109
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  60.28 kb
  allocs estimate:  229
  minimum time:     43.93 ms (0.00% GC)
  median time:      45.35 ms (0.00% GC)
  mean time:        46.25 ms (0.00% GC)
  maximum time:     51.02 ms (0.00% GC)
```

Then I removed the aggregates from inside the query handling and replaced them with the early exit optimization. Disappointingly, it didn't seem to affect performance much. Perhaps the runtime is dominated by the early table scans.

### 2016 Sep 9

I'm back from my climbing trip now, so Imp dev should return to it's usual rhythm. 

First thing is a syntactic tweak - moving returned variables to the end of the query. It's cleaner visually and it removes a lot of punctuation.

``` julia 
function who_is_metal()
  @query begin
    playlist(playlist, "Heavy Metal Classic")
    playlist_track(playlist, track)
    track(track, _, album)
    album(album, _, artist)
    artist(artist, artist_name)
    return (artist_name::String,)
  end
end
```

I'm also playing around with nesting queries for aggregation.

``` julia 
function cost_of_playlist()
  @query begin
    playlist(p, pn)
    tracks = @query begin 
      p = p
      playlist_track(p, t)
      track(t, _, _, _, _, _, _, _, price)
      return (t::Int64, price::Float64)
    end
    total = sum(tracks.columns[1])
    return (pn::String, total::Float64)
  end
end
```

I don't like the current implementation at all. It allocates a new relation on each loop, only to aggregate over it and throw it away. I don't want to get bogged down in this forever though, so I'm going to leave it for now and revisit it when I look at factorizing queries.

That `p = p` is caused by a scoping issue. I can't tell at the moment whether `p` is being used as a variable or as a constant from an outside scope, so I have to create a new `p` to resolve the ambiguity. The ideal way to fix this would be if macros could query what variables are defined in their enclosing scope, but I think this may be impossible in Julia because declarations can float upwards - a later macro could create a new variable that is available in this macro. So instead I'll just explicitly escape variables eg `$p` for constant, `p` for variable. 

I also fixed a minor bug that caused a crash on queries that don't return any results. I've been aware of it for a while but it was only worth fixing once I started working on sub-queries.

With several more hours of unrecorded bug-fixing, I finally have a working version!

``` julia
function run(num_x, num_y, num_mines)
  srand(999)
  
  @relation state() = Symbol
  @relation mine(Int64, Int64)
  @relation mine_count(Int64, Int64) = Int64
  @relation cleared(Int64, Int64)
  @relation clicked(Int64) = (Int64, Int64)
  @relation display() = Hiccup.Node
  
  @merge! state begin 
    s = :game_in_progress
    return (s::Symbol,)
  end
  
  @fix! mine begin 
    mines = @query begin
      mine(x, y)
      return (x::Int64, y::Int64)
    end
    @when length(mines) < num_mines
    x = rand(1:num_x)
    y = rand(1:num_y)
    return (x::Int64, y::Int64)
  end 
  
  @merge! mine_count begin 
    x in 1:num_x
    y in 1:num_y
    neighbouring_mines = @query begin
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when (nx != x) || (ny != y)
      mine(nx, ny) 
      return (nx::Int64, ny::Int64)
    end
    c = length(neighbouring_mines)
    return (x::Int64, y::Int64, c::Int64)
  end
  
  @Window(clicked) do window, event_number
    
    @merge! cleared begin
      clicked($event_number, x, y)
      return (x::Int64, y::Int64)
    end
    
    @fix! cleared begin 
      cleared(x,y)
      mine_count(x,y,0)
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when nx in 1:num_x
      @when ny in 1:num_y
      @when (nx != x) || (ny != y)
      return (nx::Int64, ny::Int64)
    end
    
    @merge! state begin 
      num_cleared = length(@query begin
        cleared(x,y)
        return (x::Int64, y::Int64)
      end)
      @when num_cleared + num_mines >= num_x * num_y
      s = :game_won
      return (s::Symbol,)
    end
    
    @merge! state begin
      clicked($event_number, x, y)
      mine(x,y)
      s = :game_lost
      return (s::Symbol,)
    end
    
    node = vbox(map(1:num_y) do y
      return hbox(map(1:num_x) do x  
        current_state = state.columns[1][1]
        is_cleared = exists(@query begin 
          cleared($x,$y) 
          e = true
          return (e::Bool,)
        end)
        is_mine = exists(@query begin 
          mine($x,$y) 
          e = true 
          return (e::Bool,)
        end)
        count = (@query begin 
          mine_count($x,$y,count)
          return (count::Int64,)
        end).columns[1][1]
        return @match (current_state, is_mine, is_cleared, count) begin
         (:game_in_progress, _, true, 0) => button("_")
         (:game_in_progress, _, true, _) => button(string(count))
         (:game_in_progress, _, false, _) => button(Dict(:onclick => @event clicked(x,y)), "X")
         (_, true, _, _) => button("💣")
         (_, false, _, _) => button(string(count))
         _ => error()
       end
     end)
   end)
    
   Blink.body!(window, node)
    
  end
  
  (state, mine, mine_count, clicked, display, cleared)
end

(state, mine, mine_count, clicked, display, cleared) = run(10, 20, 10)
```

The Imp computation takes around 0.1-3.0ms per event, where the top end is down to `@fix!` not doing semi-naive evaluation. Building the Hiccup node takes up to 100ms, which is disgraceful. About half of that is the repeated inner queries and the other half is entirely inside Hiccup.

There's a ton of minor stuff to fix before I'll consider this finished:

* Queries support `relation(keys) => value` syntax
* Declare merge in return statement
* Remove need for type declarations in merged returns
* Allow returning expressions, not just symbols 
* Sort only by keys in query, so that we can have non-sortable objects as values
* Distinguish empty set from set containing empty tuple 

I also want to reduce the noise of inner aggregates, but I have no good ideas right now.

### 2016 Sep 11

I figured out how to hook into Julia's type inference at runtime. The simple way is:

``` julia
Base.return_types(() -> ..., [])[1]
```

This executes at runtime and Julia can't infer the return type. I think that works fine if I move it outside the function that wraps the query. 

Another option is:

``` julia 
@generated function infer_type(closure, args...)
  m = Core.Inference._methods_by_ftype(Tuple{closure, (arg.parameters[1] for arg in args)...}, 1)[1]
  _, ty, inferred = Core.Inference.typeinf(m[3], m[1], m[2], false)
  t = inferred ? ty : Any
  quote
    $(Expr(:meta, :inline))
    $t
  end
end
```

This returns the correct results when I execute it by itself, but if I wrap it in a function call it always returns `Any`. No idea why.

Also, sometimes it segfaults. I guess there is a reason that the exported reflection functions refuse to work inside `@generated` :D

I could just use `Base.return_types` at compile time, but macros run before later functions in the same module are compiled, so it wouldn't know the types of those functions. Evalling the same module twice would produce different type inferences.

If I use it at runtime, the query itself would be fine because it's wrapped in a function, but the return type of the query would be un-inferrable.

I spent a lot of time thinking about this and eventually realised that Julia's array comprehensions have exactly the same problem - giving a stable type to the empty array. So I have a truly glorious hack:

``` julia
# nearly works, but x and y get boxed
results_x = [x for _ in []]
results_y = [y for _ in []]
x = 3
y = x + 1

# actually works!
f_x = () -> 3
f_y = (x) -> x + 1
results_x = [f_x() for _ in []]
results_y = [f_y(results_x[1]) for _ in []]
x = 3
y = x + 1
```

It's a bit sketchy, because the Julia folks keep warning everyone that type inference is not stable/predictable and that runtime behaviour shouldn't depend on inference. But as far as I'm concerned, allocating ints on the stack vs making a bajillion heap allocations *is* important runtime behavior, so I'm *already* a hostage to type inference and/or manual assertions. Throwing an error on pushing to a weirdly typed array is much less annoying than trying to allocate 100gb of Int64 objects and crashing my machine.

The actual implementation of this plan was simple enough, but the type inference really struggles with subqueries and I can't figure out why. A large part of the problem is that the generated code is pretty verbose, so it's really hard for me to work through the lowered, inferred ast and find problems. I think I'm going to abandon inference for now. I'll finish the rest of the bullet points, then clean up the compiler and then try inference again. 

### 2016 Sep 12

Let's figure out what I want the emitted code to look like.

``` julia
@query begin
  playlist(p, pn)
  tracks = @query begin 
    playlist_track($p, t)
    track(t, _, _, _, _, _, _, _, price)
    return (t::Int64, price::Float64)
  end
  total = sum(tracks.columns[1])
  return (pn::String, total::Float64)
end
```

``` julia
index_playlist = index(esc(playlist), [1,2])

columns_p = tuple(index_playlist[1])
infer_p() = infer(columns_p)

columns_pn = tuple(index_playlist[2])
infer_pn() = infer(columns_pn)

begin # subquery init
  index_playlist_track = index(esc(playlist_track), [1, 2])
  index_track = index(esc(track), [1, 9])
  
  @inline function eval_tmp1(p) = p
  columns_tmp1 = tuple(index_playlist_track[1])
  infer_tmp1() = infer(columns_tmp1, infer_p())

  columns_t = tuple(index_playlist_track[2], index_track[1])
  infer_t() = infer(columns_t)

  columns_price = tuple(index_track[9])
  infer_price() = infer(columns_price)
  
  @inline function query2_outer(results_t, results_price, p)
    for tmp1 in intersect(columns_tmp1, eval_tmp1(p))
      for t in intersect(columns_t)
        for price in intersect(columns_price)
          query2_inner(results_t, results_price, p, tmp1, t, price)
        end
      end
    end
    Relation(tuple(results_t, results_price), tuple()) # dedup
  end

  @inline function query2_inner(results_t, results_price, p, tmp1, t, price)
    push!(results_t, t)
    push!(results_price, price)
    return
  end
end

@inline eval_tracks(p) = query2_outer([infer_t() for _ in []], [infer_price() for _ in []], p)
columns_tracks = tuple()
infer_tracks() = infer(columns_tracks, eval_tracks(infer_p()))

eval_total(tracks) = sum(track.columns[1])
columns_total = tuple()
infer_total() = infer(columns_totals, eval_total(infer_tracks()))

@inline function query1_outer(results_pn, results_total)
  for p in intersect(columns_p)
    for pn in intersect(columns_pn)
      for tracks in intersect(columns_tracks, eval_tracks(p))
        for total in intersect(columns_total, eval_total(tracks))
          query1_inner(results_pn, results_total, p, pn, tracks, total)
        end
      end
    end
  end
  Relation((results_pn, results_total)) # dedup
end

@inline function query1_inner(results_pn, results_total, p, pn, tracks, total)
  push!(results_pn, pn)
  push!(results_total, total)
  return
end

query2_outer([infer_pn() for _ in []], [infer_total() for _ in []])
```

The first step is to clean up the compiler itself, while still generating the same code. I've got the bulk of this done but it'll need some debugging tomorrow.

### 2016 Sep 13

I have the cleaned up compiler working now, and it's a relief to have done it. 

However, the generated code is not identical - there is a significant slowdown in some of the JOB queries. It looks like the cause is that I'm no longer lifting constants to the top of the variable order. Easily fixed.

Here is the new compiler code, in all it's commented glory:

``` julia
function plan_join(query)
  # parse
  clauses = []
  for line in query.args
    clause = @match line begin
      line::Symbol => Hint(line)
      Expr(:call, [:in, var, expr], _) => In(var, expr, collect_vars(expr))
      Expr(:call, [name, vars...], _) => Row(name, Any[vars...])
      Expr(:(=), [var, expr], _) => Assign(var, expr, collect_vars(expr))
      Expr(:macrocall, [head, expr], _), if head == Symbol("@when") end => When(expr, collect_vars(expr))
      Expr(:return, [Expr(:tuple, [vars...], _)], _) => Return((), map(get_var_symbol, vars), map(get_var_type, vars))
      Expr(:return, [Expr(:call, [:tuple, vars...], _)], _) => Return((), map(get_var_symbol, vars), map(get_var_type, vars))
      Expr(:return, [Expr(:call, [name, vars...], _)], _) => Return(name, map(get_var_symbol, vars), map(get_var_type, vars))
      Expr(:line, _, _) => ()
      _ => error("Confused by: $line")
    end
    if clause != ()
      push!(clauses, clause)
    end
  end
  
  # check all assignments are to single vars
  for clause in clauses
    if typeof(clause) in [In, Assign]
      @assert isa(clause.var, Symbol)
    end
  end
  
  # add a return if needed
  returns = [clause for clause in clauses if typeof(clause) == Return]
  if length(returns) == 0
    return_clause = Return((), [], [])
  elseif length(returns) == 1
    return_clause = returns[1]
  else
    error("Too many returns: $returns")
  end
  
  # rewrite expressions nested in Row
  old_clauses = clauses
  clauses = []
  for clause in old_clauses
    if typeof(clause) in [Row]
      for (ix, expr) in enumerate(clause.vars)
        if !isa(expr, Symbol)
          var = gensym("constant")
          clause.vars[ix] = var
          value = @match expr begin
            Expr(:$, [value], _) => value
            value => value 
          end
          insert!(clauses, 1, Assign(var, value, collect_vars(value)))
        end
      end
    end
    push!(clauses, clause)
  end
  
  # collect vars created in this query
  created_vars = Set()
  for clause in clauses
    if typeof(clause) in [Row]
      for var in clause.vars
        push!(created_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In]
      push!(created_vars, clause.var)
    end
  end
  delete!(created_vars, :_) # _ is a wildcard, not a real var
  
  # collect vars mentioned in this query, in order of mention
  mentioned_vars = []
  for clause in clauses 
    if typeof(clause) in [Row, When, Assign, In]
      for var in clause.vars
        push!(mentioned_vars, var)
      end 
    end
    if typeof(clause) in [Assign, In, Hint]
      push!(mentioned_vars, clause.var)
    end
  end
  
  # use mention order to decide execution order
  vars = unique((var for var in mentioned_vars if var in created_vars))
  
  # collect clauses that assign a value to a var before intersect
  var_assigned_by = Dict()
  for clause in clauses
    if typeof(clause) in [Assign, In]
      @assert !haskey(var_assigned_by, clause.var) # only one assignment per var 
      var_assigned_by[clause.var] = clause
    end
  end
  
  # for each var, collect list of relation/column pairs that need to be intersected
  sources = Dict(var => Tuple{Int64, Int64}[] for var in vars)
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      for (var_ix, var) in enumerate(clause.vars)
        if var != :_
          push!(sources[var], (clause_ix, var_ix))
        end
      end
    end
  end
  
  # for each Row clause, figure out what order to sort the index in
  sort_orders = Dict(clause_ix => Int64[] for clause_ix in 1:length(clauses))
  for var in vars 
    for (clause_ix, var_ix) in sources[var]
      push!(sort_orders[clause_ix], var_ix)
    end
  end
  
  # assign a slot in the los/ats/his arrays for each relation/column pair
  ixes = Tuple{Int64, Any}[]
  for (clause_ix, var_ixes) in sort_orders
    for var_ix in var_ixes
      push!(ixes, (clause_ix, var_ix))
    end
    push!(ixes, (clause_ix, :buffer))
  end
  ix_for = Dict(column => ix for (ix, column) in enumerate(ixes))
  
  # --- codegen ---
  
  # for each Row clause, get the correct index
  index_inits = []
  for (clause_ix, clause) in enumerate(clauses)
    if typeof(clause) == Row 
      order = sort_orders[clause_ix]
      index_init = :($(Symbol("index_$clause_ix")) = index($(Symbol("relation_$clause_ix")), $order))
      push!(index_inits, index_init)
    end
  end
  
  # for each var, collect up the columns to be intersected
  columns_inits = []
  for var in vars
    columns = [:($(Symbol("index_$clause_ix"))[$var_ix]) for (clause_ix, var_ix) in sources[var]]
    columns_init = :($(Symbol("columns_$var")) = [$(columns...)])
    push!(columns_inits, columns_init)
  end
  
  # for each var, make list of ixes into the global state
  ixes_inits = []
  for var in vars 
    ixes_init = :($(Symbol("ixes_$var")) = $([ix_for[source] for source in sources[var]]))
    push!(ixes_inits, ixes_init)
  end
  
  # initialize arrays for storing results
  results_inits = []
  for (ix, var) in enumerate(return_clause.vars) 
    if return_clause.name == ()
      typ = return_clause.typs[ix]
    else
      typ = :(eltype($(esc(return_clause.name)).columns[$ix]))
    end
    result_init = :($(Symbol("results_$var")) = Vector{$typ}())
    push!(results_inits, result_init)
  end
  
  # initilize global state
  los = [1 for _ in ixes]
  ats = [1 for _ in ixes]
  his = []
  for (clause_ix, var_ix) in ixes
    if var_ix == :buffer
      push!(his, 0)
    else 
      push!(his, :(length($(Symbol("index_$clause_ix"))[$var_ix]) + 1))
    end
  end
  
  # combine all the init steps
  init = quote
    $(index_inits...)
    $(columns_inits...)
    $(ixes_inits...)
    $(results_inits...)
    los = [$(los...)]
    ats = [$(ats...)]
    his = [$(his...)]
  end
  
  # figure out at which point in the variable order each When clause can be run
  whens = [[] for _ in vars]
  for clause in clauses
    if typeof(clause) == When
      var_ix = maximum(indexin(collect_vars(clause.expr), vars))
      push!(whens[var_ix], clause.expr)
    end
  end
  
  # figure out at which point in the variable order we have all the variables we need to return
  return_after = maximum(push!(indexin(return_clause.vars, vars), 0))
  
  # store results 
  body = quote
    $([:(push!($(Symbol("results_$var")), $(esc(var))))
    for var in return_clause.vars]...)
    need_more_results = false
  end
  
  # build up the main loop from the inside out
  for var_ix in length(vars):-1:1
    var = vars[var_ix]
    var_columns = Symbol("columns_$var")
    var_ixes = Symbol("ixes_$var")
    
    # run any When clauses
    for when in whens[var_ix]
      body = :(if $(esc(when)); $body; end)
    end
    
    # after return_after, only need to find one solution, not all solutions
    if var_ix == return_after
      body = quote
        need_more_results = true
        $body
      end
    end
    need_more_results = var_ix > return_after ? :need_more_results : true
    
    # find valid values for this variable
    clause = get(var_assigned_by, var, ())
    if typeof(clause) == Assign
      body = quote
        let $(esc(var)) = $(esc(clause.expr))
          if assign($var_columns, los, ats, his, $var_ixes, $(esc(var)))
            $body
          end
        end
      end
    elseif typeof(clause) == In
      body = quote 
        let
          local iter = $(esc(clause.expr))
          local state = start(iter)
          local $(esc(var)) 
          while $need_more_results && !done(iter, state)
            ($(esc(var)), state) = next(iter, state)
            if assign($var_columns, los, ats, his, $var_ixes, $(esc(var)))
              $body
            end
          end
        end
      end
    else 
      result_column = ix_for[sources[var][1]]
      body = quote
        start_intersect($var_columns, los, ats, his, $var_ixes)
        while $need_more_results && next_intersect($var_columns, los, ats, his, $var_ixes)
          let $(esc(var)) = $(Symbol("columns_$var"))[1][los[$(result_column+1)]]
            $body
          end
        end
      end 
    end
    
  end
  
  query_symbol = gensym("query")
  relation_symbols = [Symbol("relation_$clause_ix") for (clause_ix, clause) in enumerate(clauses) if typeof(clause) == Row]
  relation_names = [esc(clause.name) for clause in clauses if typeof(clause) == Row]
  result_symbols = [Symbol("results_$var") for var in return_clause.vars]
          
  code = quote 
    function $query_symbol($(relation_symbols...))
      $init
      $body
      Relation(tuple($(result_symbols...)))
    end
    $query_symbol($(relation_names...))
  end
  
  (code, return_clause)
end

function plan_query(query)
  (join, return_clause) = plan_join(query)
  
  (project, _) = plan_join(quote 
    intermediate($(return_clause.vars...))
    return intermediate($(return_clause.vars...)) # returning to intermediate is just a temporary hack to convey types
  end)
  
  quote 
    let $(esc(:intermediate)) = let; $join; end
      $((return_clause.name == ()) ? project : :(merge!($(esc(return_clause.name)), $project)))
    end
  end
end
```

Now to start cleaning up the generated code. First step is to remove the second join and do the deduping in the `Relation` constructor instead. 

``` julia
function Relation{T}(columns::T)
  deduped = tuple((Vector{eltype(column)}() for column in columns)...)
  quicksort!(columns)
  at = 1
  hi = length(columns[1])
  while at <= hi
    push_in!(deduped, columns, at)
    while (at += 1; (at <= hi) && cmp_in(columns, columns, at, at-1) == 0) end
  end
  order = collect(1:length(columns))
  key_types = Type[eltype(column) for column in columns]
  Relation(deduped, Dict{Vector{Int64},T}(order => deduped), key_types, Type[])
end
```

Some slight slowdowns on queries that produce a large number of intermediate results, but nothing that bothers me too much:

``` julia
@benchmark(q1a()) = BenchmarkTools.Trial: 
  samples:          3016
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  7.28 kb
  allocs estimate:  66
  minimum time:     1.25 ms (0.00% GC)
  median time:      1.66 ms (0.00% GC)
  mean time:        1.66 ms (0.00% GC)
  maximum time:     3.17 ms (0.00% GC)
@benchmark(q2a()) = BenchmarkTools.Trial: 
  samples:          73
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  318.03 kb
  allocs estimate:  3730
  minimum time:     67.10 ms (0.00% GC)
  median time:      68.82 ms (0.00% GC)
  mean time:        69.36 ms (0.46% GC)
  maximum time:     95.58 ms (24.48% GC)
@benchmark(q3a()) = BenchmarkTools.Trial: 
  samples:          43
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  11.31 kb
  allocs estimate:  81
  minimum time:     113.73 ms (0.00% GC)
  median time:      115.70 ms (0.00% GC)
  mean time:        116.35 ms (0.00% GC)
  maximum time:     120.57 ms (0.00% GC)
@benchmark(q4a()) = BenchmarkTools.Trial: 
  samples:          108
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  21.92 kb
  allocs estimate:  98
  minimum time:     44.07 ms (0.00% GC)
  median time:      46.17 ms (0.00% GC)
  mean time:        46.55 ms (0.00% GC)
  maximum time:     53.74 ms (0.00% GC)
```

### 2016 Sep 14

My latest attempt at type inference spawned a two day debugging session. I was very confused for a long time by the following situation.

``` julia
infer_p() # inferred result type is Int64
infer_tracks(1::Int64) # inferred result type is Relation{Int64, Float64}
infer_tracks(infer_p()) # inferred result type is Any
```

I finally found the bug. At some point I had typed `$(Symbol("infer_$var()"))` rather than `$(Symbol("infer_$var"))()`. The latter creates a call to the function `infer_p`. The former creates a load of the variable `infer_p()`, which is nonsense. But both print the same way when the ast is rendered! And, weirdly, the type inference for the former produced `Any`, instead of producing `Union{}` which would have clued me in to the fact that I was producing nonsense code.
  
But it's fixed now. I have type inference.

``` julia
const num_x = 3
const num_y = 4

@relation mine(Int64, Int64)

function neighbours()
  @query begin 
    x in 1:num_x
    y in 1:num_y
    neighbouring_mines = @query begin
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when (nx != x) || (ny != y)
      mine(nx, ny) 
    end
    c = length(neighbouring_mines)
    return (x, y) => c
  end
end

Base.return_types(neighbours) 
# [Data.Relation{Tuple{Array{Int64,1},Array{Int64,1},Array{Int64,1}}}]
```

I also fixed all the bullet points from the minesweeper post. The only remaining problem is that hiccup nodes are not comparable, so I can't sort them into a relation column.

### 2016 Sep 15

So many strange bugs today. One query only gets the correct inferred type if the module is compiled twice. That makes no sense.

Ignoring that for now. Node comparisons:

``` julia
function Base.cmp{T1, T2}(n1::Hiccup.Node{T1}, n2::Hiccup.Node{T2})
  c = cmp(T1, T2)
  if c != 0; return c; end
  c = cmp(length(n1.attrs), length(n2.attrs))
  if c != 0; return c; end
  for (a1, a2) in zip(n1.attrs, n2.attrs)
    c = cmp(a1, a2)
    if c != 0; return c; end
  end
  c = cmp(length(n1.children), length(n2.children))
  if c != 0; return c; end
  for (c1, c2) in zip(n1.children, n2.children)
    c = cmp(c1, c2)
    if c != 0; return c; end
  end
  return 0
end
```

Which means the UI can now happen in Imp too. Here is the whole minesweeper:

``` julia 
@relation state() => Symbol
@relation mine(Int64, Int64)
@relation mine_count(Int64, Int64) => Int64
@relation cleared(Int64, Int64)
@relation clicked(Int64) => (Int64, Int64)

@relation cell(Int64, Int64) => Hiccup.Node
@relation row(Int64) => Hiccup.Node
@relation grid() => Hiccup.Node

@query begin 
  return state() => :game_in_progress
end

while length(@query mine(x,y)) < num_mines
  @query begin 
    nx = rand(1:num_x)
    ny = rand(1:num_y)
    return mine(nx, ny)
  end 
end

@query begin 
  x in 1:num_x
  y in 1:num_y
  neighbouring_mines = @query begin
    nx in (x-1):(x+1)
    ny in (y-1):(y+1)
    @when (nx != x) || (ny != y)
    mine(nx, ny) 
  end
  c = length(neighbouring_mines)
  return mine_count(x, y) => c
end

@Window(clicked) do window, event_number

  @query begin
    clicked($event_number) => (x, y)
    return cleared(x, y)
  end
  
  fix(cleared) do
    @query begin
      cleared(x,y)
      mine_count(x,y) => 0
      nx in (x-1):(x+1)
      ny in (y-1):(y+1)
      @when nx in 1:num_x
      @when ny in 1:num_y
      @when (nx != x) || (ny != y)
      return cleared(nx, ny)
    end
  end
  
  @query begin 
    num_cleared = length(@query cleared(x,y))
    @when num_cleared + num_mines >= num_x * num_y
    return state() => :game_won
  end
  
  @query begin
    clicked($event_number) => (x, y)
    mine(x,y)
    return state() => :game_lost
  end
  
  @query begin
    state() => current_state
    x in 1:num_x
    y in 1:num_y
    is_cleared = exists(@query cleared($x,$y))
    is_mine = exists(@query mine($x,$y))
    mine_count(x, y) => count
    cell_node = (@match (current_state, is_mine, is_cleared, count) begin
      (:game_in_progress, _, true, 0) => button("_")
      (:game_in_progress, _, true, _) => button(string(count))
      (:game_in_progress, _, false, _) => button(Dict(:onclick => @event clicked(x,y)), "X")
      (:game_won, true, _, _) => button("💣")
      (:game_lost, true, _, _) => button("☀")
      (_, false, _, _) => button(string(count))
      other => error("The hell is this: $other")
    end)::Hiccup.Node
    return cell(x,y) => cell_node
  end
  
  @query begin
    y in 1:num_y
    row_node = hbox((@query cell(x,$y) => cell_node).columns[3])::Hiccup.Node
    return row(y) => row_node
  end
  
  @query begin
    grid_node = vbox((@query row(y) => row_node).columns[2])::Hiccup.Node
    return grid() => grid_node
  end
  
  Blink.body!(window, grid.columns[1][1])
  
end
```

Still a couple of ugly parts. 

* Those `::Hiccup.Node`s are necessary because the compiler infers `Union{Hiccup.Node, Void}` otherwise. Haven't figured that out yet.

* `.columns[3]` should just be `[3]` or `.cell_node`. The latter will work if I store variable names in the relation and implement `getfield`. Just have to be careful not to break inference.

* The call to `Blink.body!` should happen in the UI lib.

The first one appears to be an inference problem. I figured out a way to simplify the inference stage. While doing that, I discovered that I had broken the JOB queries some time ago and somehow not noticed. I could bisect the problem, but I've changed the representation of relations a few times in the past days so I would have to rebuild the JOB dataset on every commit, which takes about 30 minutes each time. 

Maybe I can just figure it out by comparing smaller queries to postgres.

The smallest query that disagrees with postgres is:

``` julia
@query begin 
  info_type_info(it_id, "top 250 rank")
  movie_info_idx_info_type_id(mii_id, it_id)
  movie_info_idx_movie_id(mii_id, t_id)
  title_production_year(t_id, t_production_year)
  # movie_companies_movie_id(mc_id, t_id)
  # movie_companies_company_type_id(mc_id, ct_id)
  # company_type_kind(ct_id, "production companies")
  # movie_companies_note(mc_id, mc_note)
  # @when !contains(mc_note, "as Metro-Goldwyn-Mayer Pictures") &&
  #   (contains(mc_note, "co-production") || contains(mc_note, "presents"))
  return (t_production_year,)
end
```

Comparing `title_production_year` to the csv I notice that the length is correct but the data is wrong. A few minutes later it all snaps into focus - the changed the Relation constructor to sort it's input data for deduping, but I share input columns in the JOB data. This was fine for a while because I save the relations on disk and it was only when I changed the representation of relations and had to regenerate the JOB data from scratch that it all went wrong.

An aliasing/mutation bug. I feel so dirty.

Back to inference. I noticed something that is probably responsible for a lot of my confusion.

``` julia
julia> function foo1()
         function bar()
           @inline eval_a() = 1
           @inline eval_b(a) = a
           a = [(([[eval_a()]])[1])[1] for _ in []]
           b = [(([[eval_b(a[1])]])[1])[1] for _ in []]
         end
         bar()
       end
foo1 (generic function with 1 method)

julia> foo1()
0-element Array{Int64,1}

julia> function foo2()
         function bar()
           @inline eval_a() = 1
           @inline eval_b(a) = a
           @show a = [(([[eval_a()]])[1])[1] for _ in []]
           @show b = [(([[eval_b(a[1])]])[1])[1] for _ in []]
         end
         bar()
       end
foo2 (generic function with 1 method)

julia> foo2()
a = [(([[eval_a()]])[1])[1] for _ = []] = Int64[]
b = [(([[eval_b(a[1])]])[1])[1] for _ = []] = Any[]
0-element Array{Any,1}
```

The `@show` macro I immediately turn to to help me debug these issues also confuses type inference itself. So as soon as I tried to debug some problem, I was creating a new problem. Hopefully I shouldn't have any more crazy self-doubting inference debugging sessions now that I've figured this out.

The problem with the buttons in minesweeper seems to be that inference just rounds up to `Any` when encountering an abstract type in an array:

``` julia
function foo()
  const bb = button("")
  const t = typeof(bb)
  x = Vector{t}()
  y = [bb]
end
```

``` julia 
Variables:
  #self#::Minesweeper.#foo
  bb::HICCUP.NODE{TAG}
  t::TYPE{_<:HICCUP.NODE}
  x::ANY
  y::ANY

Body:
  begin 
      bb::HICCUP.NODE{TAG} = $(Expr(:invoke, LambdaInfo for #button#1(::Array{Any,1}, ::Function, ::String, ::Vararg{String,N}), :(Minesweeper.#button#1), :((Core.ccall)(:jl_alloc_array_1d,(Core.apply_type)(Core.Array,Any,1)::Type{Array{Any,1}},(Core.svec)(Core.Any,Core.Int)::SimpleVector,Array{Any,1},0,0,0)::Array{Any,1}), :(Minesweeper.button), "")) # line 167:
      t::TYPE{_<:HICCUP.NODE} = (Minesweeper.typeof)(bb::HICCUP.NODE{TAG})::TYPE{_<:HICCUP.NODE} # line 168:
      x::ANY = ((Core.apply_type)(Minesweeper.Vector,t::TYPE{_<:HICCUP.NODE})::TYPE{_<:ARRAY{_<:HICCUP.NODE,1}})()::ANY # line 169:
      SSAValue(0) = (Base.vect)(bb::HICCUP.NODE{TAG})::ANY
      y::ANY = SSAValue(0)
      return SSAValue(0)
  end::ANY
```

So I fix that by just using tuples instead which don't have any weird type coercion behavior. And finally we get to the root of the issue:

``` julia
# inferred types
cell.columns[3] # Array{Hiccup.Node,1}
cell.columns[3][1] # HICCUP.NODE{TAG}
[cell.columns[3][1]] # ANY
```

`ANY` is not quite the same as `Any`. In user annotations, the former indicates not to specialize on this type at all. I'm guessing that the all-caps in `HICCUP.NODE{TAG}` means something similar. 

So the core issue seems to be that if you have an array of an abstract type and you take something out of it and put it in a new array, Julia just bails out entirely. I don't know why it behaves this way, but I can at least work around it by just never taking anything out of an array during inference. I make the results vectors like this now:

``` julia
type_x = eltype([eval_x(results_y[1]) for _ in []])
results_x = Vector{typejoin(type_x, eltype(index_2[3]), eltype(index_4[3]))}()
```

All my tests pass and none of them require type annotations. I've also added new tests that look at the inferred return types for all the old tests to make sure it stays that way.

### 2016 Sep 16

I spent today mostly deciding what to do next. I want to publish some sort of progress report before the end of the month, so I spent a few hours drafting that report. Chipping away at all the caveats in that report gives me a nice todo list for the next few weeks.

### 2016 Sep 17

I spent a few hours getting the JOB data into SQLite, figuring out how to use SQLite.jl and running benchmarks. 

``` julia
import SQLite
function bench_sqlite()
  db = SQLite.DB("../job/job.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = 1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  medians = []
  for q in 1:4
    query = rstrip(readline("../job/$(q)a.sql"))
    @time SQLite.query(db, query)
    trial = @show @benchmark SQLite.query($db, $query)
    push!(medians, @show (median(trial.times) / 1000000))
  end
  medians
end
```

For postgres I'm instead just running the queries through bash, but using the execution times from EXPLAIN ANALYZE instead of the @benchmark time. 

``` julia 
function bench_pg()
  medians = []
  for q in 1:4
    query = rstrip(readline("../job/$(q)a.sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    bench = "explain analyze $query"
    cmd = `sudo -u postgres psql -c $bench`
    times = Float64[]
    @show q
    @show @benchmark push!($times, parse(Float64, match(r"Execution time: (\S*) ms", readstring($cmd))[1]))
    push!(medians, @show median(times))
  end
  medians
end
```

I've also changed the Imp JOB data to use exactly the same schema as the other databases, so queries now look like:

``` julia 
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
```

They're all broken now though, so it's debugging time.

I added some tests against SQLite to help out.

``` julia 
db = SQLite.DB("../job/job.sqlite")
for q in 1:4
  results_imp = eval(Symbol("q$(q)a"))()
  query = rstrip(readline("../job/$(q)a.sql"))
  query = replace(query, "MIN", "")
  frame = SQLite.query(db, query)
  num_columns = length(results_imp.columns)
  results_sqlite = Relation(tuple((frame[ix].values for ix in 1:num_columns)...), num_columns)
  @show q
  @test length(results_imp.columns[1]) == length(results_sqlite.columns[1])
  @test results_imp.columns == results_sqlite.columns
end
```

### 2016 Sep 19

Only have a few hours today. Debugging time. 

Q1 and Q3 don't work. Q2 and Q4 do. All of them worked before I changed the schema and rewrote the queries to match. 

I want to narrow the failure down to a single incorrect row, so let's add:

``` julia 
function diff_sorted!{T <: Tuple, K <: Tuple}(old::T, new::T, old_key::K, new_key::K, old_only::T, new_only::T)
  @inbounds begin
    old_at = 1
    new_at = 1
    old_hi = length(old[1])
    new_hi = length(new[1])
    while old_at <= old_hi && new_at <= new_hi
      c = cmp_in(old_key, new_key, old_at, new_at)
      if c == 0
        old_at += 1
        new_at += 1
      elseif c == 1
        push_in!(new_only, new, new_at)
        new_at += 1
      else 
        push_in!(old_only, old, old_at)
        old_at += 1
      end
    end
    while old_at <= old_hi
      push_in!(old_only, old, old_at)
      old_at += 1
    end
    while new_at <= new_hi
      push_in!(new_only, new, new_at)
      new_at += 1
    end
  end
end

function diff{T}(old::Relation{T}, new::Relation{T})
  @assert old.num_keys == new.num_keys 
  order = collect(1:length(old.columns))
  old_index = index(old, order)
  new_index = index(new, order)
  old_only_columns = tuple([Vector{eltype(column)}() for column in old.columns]...)
  new_only_columns = tuple([Vector{eltype(column)}() for column in new.columns]...)
  diff_sorted!(old_index, new_index, old_index[1:old.num_keys], new_index[1:new.num_keys], old_only_columns, new_only_columns)
  old_only_indexes = Dict{Vector{Int64}, typeof(old_only_columns)}(order => old_only_columns)
  new_only_indexes = Dict{Vector{Int64}, typeof(new_only_columns)}(order => new_only_columns)
  (Relation(old_only_columns, old.num_keys, old_only_indexes), Relation(new_only_columns, new.num_keys, new_only_indexes))
end
```

And change the test to:

``` julia
(imp_only, sqlite_only) = Data.diff(results_imp, results_sqlite)
@test imp_only.columns == sqlite_only.columns # ie both empty - but @test will print both otherwise
```

And for Q1 we get:

``` julia
Test Failed
  Expression: imp_only.columns == sqlite_only.columns
   Evaluated: (String["(as A Selznick International Picture) (as Selznick International presents its picturization of Daphne Du Maurier's celebrated novel also)","(as Warner Bros.- Seven Arts presents)","(presents: Ernest Lehman's production of Edward Albee's)"],String["Rebecca","The Wild Bunch","Who's Afraid of Virginia Woolf?"],[1940,1969,1966]) == (String[],String[],Int64[])
```

I have a sudden suspicion. Maybe I don't track ixes correctly when there are `_`s in between variables.

Let's dump the ixes for Q1. 

``` julia
ixes = Tuple{Int64,Any}[(7,2),(7,1),(7,:buffer),(4,3),(4,2),(4,:buffer),(9,:buffer),(2,:buffer),(3,2),(3,1),(3,:buffer),(5,1),(5,2),(5,5),(5,:buffer),(8,:buffer),(6,2),(6,4),(6,5),(6,:buffer),(1,:buffer)]
```

Nope, looks fine. Back to systematic debugging.

Let's first see if those rows actually exist in the Imp tables.

``` julia
for ix in 1:length(title)
  if title.columns[2][ix] == "Who's Afraid of Virginia Woolf?"
    @show title.columns[1][ix] title.columns[5][ix]
  end
end

# (title.columns[1])[ix] = 2499084
# (title.columns[5])[ix] = 1966

for ix in 1:length(movie_companies)
  if movie_companies.columns[2][ix] == 2499084
    @show movie_companies.columns[5][ix]
  end
end

# (movie_companies.columns[5])[ix] = "(1973) (USA) (TV) (original airing)"
# (movie_companies.columns[5])[ix] = "(1976) (Finland) (TV)"
# (movie_companies.columns[5])[ix] = "(1966) (USA) (theatrical)"
# (movie_companies.columns[5])[ix] = "(1966) (Finland) (theatrical)"
# (movie_companies.columns[5])[ix] = "(2004) (Czech Republic) (theatrical)"
# (movie_companies.columns[5])[ix] = "(1966) (West Germany) (theatrical)"
# (movie_companies.columns[5])[ix] = "(2006) (Canada) (DVD) (4 film set)"
# (movie_companies.columns[5])[ix] = "(19??) (West Germany) (VHS)"
# (movie_companies.columns[5])[ix] = "(2006) (Germany) (DVD)"
# (movie_companies.columns[5])[ix] = "(2007) (Finland) (DVD)"
# (movie_companies.columns[5])[ix] = "(2007) (Netherlands) (DVD)"
# (movie_companies.columns[5])[ix] = "(1966) (Sweden) (VHS)"
# (movie_companies.columns[5])[ix] = "(1992) (USA) (video) (laserdisc)"
# (movie_companies.columns[5])[ix] = "(1994) (USA) (VHS)"
# (movie_companies.columns[5])[ix] = "(1997) (USA) (DVD)"
# (movie_companies.columns[5])[ix] = "(2000) (USA) (VHS)"
# (movie_companies.columns[5])[ix] = "(2006) (USA) (DVD) (4 film set)"
# (movie_companies.columns[5])[ix] = "(2006) (USA) (DVD) (two-disc special edition)"
# (movie_companies.columns[5])[ix] = "(1966) (Sweden) (theatrical)"
# (movie_companies.columns[5])[ix] = "(uncredited)"
# (movie_companies.columns[5])[ix] = "(presents: Ernest Lehman's production of Edward Albee's)"
```

So that looks legit. What about the info?

``` julia
for ix in 1:length(info_type)
  if info_type.columns[2][ix] == "top 250 rank"
    @show info_type.columns[1][ix]
  end
end

# (info_type.columns[1])[ix] = 112

for ix in 1:length(movie_info_idx)
  if movie_info_idx.columns[2][ix] == 2499084 && movie_info_idx.columns[3][ix] == 112
    @show movie_info_idx.columns[1][ix]
  end
end

(movie_info_idx.columns[1])[ix] = 1379970
```

Wait, so it does match the whole query. Let's just confirm that in postgres too.

```
postgres=# select distinct info_type.info, title.title from title, movie_info_idx, info_type where info_type.info = 'top 250 rank' and title.id = 2499084 and info_type.id = movie_info_idx.info_type_id and movie_info_idx.movie_id = title.id;
     info     |              title              
--------------+---------------------------------
 top 250 rank | Who's Afraid of Virginia Woolf?
```

```
sqlite> select distinct info_type.info, title.title from title, movie_info_idx, info_type where info_type.info = 'top 250 rank' and title.id = 2499084 and info_type.id = movie_info_idx.info_type_id and movie_info_idx.movie_id = title.id;
top 250 rank|Who's Afraid of Virginia Woolf?
sqlite> 
```

Oh, did I check the company type?

``` julia
for ix in 1:length(movie_companies)
  if movie_companies.columns[2][ix] == 2499084
    @show movie_companies.columns[4][ix] movie_companies.columns[5][ix]
  end
end

# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1973) (USA) (TV) (original airing)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1976) (Finland) (TV)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1966) (USA) (theatrical)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1966) (Finland) (theatrical)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2004) (Czech Republic) (theatrical)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1966) (West Germany) (theatrical)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2006) (Canada) (DVD) (4 film set)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(19??) (West Germany) (VHS)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2006) (Germany) (DVD)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2007) (Finland) (DVD)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2007) (Netherlands) (DVD)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1966) (Sweden) (VHS)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1992) (USA) (video) (laserdisc)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1994) (USA) (VHS)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1997) (USA) (DVD)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2000) (USA) (VHS)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2006) (USA) (DVD) (4 film set)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(2006) (USA) (DVD) (two-disc special edition)"
# (movie_companies.columns[4])[ix] = 1
# (movie_companies.columns[5])[ix] = "(1966) (Sweden) (theatrical)"
# (movie_companies.columns[4])[ix] = 2
# (movie_companies.columns[5])[ix] = "(uncredited)"
# (movie_companies.columns[4])[ix] = 2
# (movie_companies.columns[5])[ix] = "(presents: Ernest Lehman's production of Edward Albee's)"

for ix in 1:length(company_type)
  if company_type.columns[1][ix] in [1,2]
    @show company_type.columns[1][ix] company_type.columns[2][ix]
  end
end

# (company_type.columns[1])[ix] = 1
# (company_type.columns[2])[ix] = "distributors"
# (company_type.columns[1])[ix] = 2
# (company_type.columns[2])[ix] = "production companies"
```

```
postgres=# select title.title, movie_companies.company_type_id from title, movie_companies where title.id = 2499084 and movie_companies.movie_id = title.id;
              title              | company_type_id 
---------------------------------+-----------------
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               1
 Who's Afraid of Virginia Woolf? |               2
 Who's Afraid of Virginia Woolf? |               2
(21 rows)

postgres=# select * from company_type where company_type.id in (1,2);
 id |         kind         
----+----------------------
  1 | distributors
  2 | production companies
(2 rows)
```

Maybe it's the LIKE patterns that I'm messing up? Let's modify Q1A to specify this particular title, return everything and then remove conditions until we get results.

```
postgres=# SELECT distinct mc.note AS production_note, (t.title) AS movie_title, (t.production_year), ct.kind, it.info AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND mc.note  not like '%(as Metro-Goldwyn-Mayer Pictures)%' and (mc.note like '%(co-production)%' or mc.note like '%(presents)%') AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id and t.id = 2499084;
 production_note | movie_title | production_year | kind | movie_year 
-----------------+-------------+-----------------+------+------------
(0 rows)

postgres=# SELECT distinct mc.note AS production_note, (t.title) AS movie_title, (t.production_year), ct.kind, it.info AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id and t.id = 2499084;
production_note                      |           movie_title           | production_year |         kind         |  movie_year  
----------------------------------------------------------+---------------------------------+-----------------+----------------------+--------------
(uncredited)                                             | Who's Afraid of Virginia Woolf? |            1966 | production companies | top 250 rank
(presents: Ernest Lehman's production of Edward Albee's) | Who's Afraid of Virginia Woolf? |            1966 | production companies | top 250 rank
(2 rows)

postgres=# SELECT distinct mc.note AS production_note, (t.title) AS movie_title, (t.production_year), ct.kind, it.info AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id and t.id = 2499084 and mc.note like '%presents%';
production_note                      |           movie_title           | production_year |         kind         |  movie_year  
----------------------------------------------------------+---------------------------------+-----------------+----------------------+--------------
(presents: Ernest Lehman's production of Edward Albee's) | Who's Afraid of Virginia Woolf? |            1966 | production companies | top 250 rank
(1 row)

postgres=# SELECT distinct mc.note AS production_note, (t.title) AS movie_title, (t.production_year), ct.kind, it.info AS movie_year FROM company_type AS ct, info_type AS it, movie_companies AS mc, movie_info_idx AS mi_idx, title AS t WHERE ct.kind = 'production companies' AND it.info = 'top 250 rank' AND ct.id = mc.company_type_id AND t.id = mc.movie_id AND t.id = mi_idx.movie_id AND mc.movie_id = mi_idx.movie_id AND it.id = mi_idx.info_type_id and t.id = 2499084 and mc.note like '%(presents)%';
 production_note | movie_title | production_year | kind | movie_year 
-----------------+-------------+-----------------+------+------------
(0 rows)
```

It appears that "(presents: Ernest Lehman's production of Edward Albee's)" is LIKE "%presents%" but not LIKE "%(presents)%". The postgres docs tell me that the parens are used for grouping patterns, but I guess that doesn't apply if there is only one alternative.

If I use parens too then I get the correct results. Fine. On to Q3.

``` julia
q = 3
length(results_sqlite) = 107
Test Failed
  Expression: imp_only.columns == sqlite_only.columns
   Evaluated: (String[],) == (String["Austin Powers 4","Teeny-Action Volume 7"],)
```

Imp returns 105 rows. SQLite returns 107 rows. Postgres returns 105 rows. That sounds like I messed up my data sources. 

Oh well, let's blow everything away and rebuild, to make sure I have a consistent set of data.

I'm using the [original csv files](http://homepages.cwi.nl/%7Eboncz/job/imdb.tgz) from the author of the JOB paper. Postgres can import them directly. SQLite doesn't understand the escapes, so I'll re-export them from postgres to feed to SQLite (this requires giving the postgres user write access to the directory). Imp can read the originals. Then if Imp and Postgres agree on the results and SQLite disagrees, we can suspect the export/import process.


``` julia
length(Job.q3a())
# 105
```

```
postgres=# SELECT count(distinct t.title) FROM keyword AS k, movie_info AS mi, movie_keyword AS mk, title AS t WHERE k.keyword  like '%sequel%' AND mi.info  IN ('Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German') AND t.production_year > 2005 AND t.id = mi.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi.movie_id AND k.id = mk.keyword_id;
 count 
-------
   105
(1 row)
```
 
```
sqlite> SELECT count(distinct t.title) FROM keyword AS k, movie_info AS mi, movie_keyword AS mk, title AS t WHERE k.keyword  like '%sequel%' AND mi.info  IN ('Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German') AND t.production_year > 2005 AND t.id = mi.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi.movie_id AND k.id = mk.keyword_id;
107
```

Well there you go. 

Let's pick one of the extra rows and see what's going on.

```
sqlite> SELECT distinct t.title, k.keyword, mi.info, t.production_year FROM keyword AS k, movie_info AS mi, movie_keyword AS mk, title AS t WHERE k.keyword  like '%sequel%' AND mi.info  IN ('Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German') AND t.production_year > 2005 AND t.id = mi.movie_id AND t.id = mk.movie_id AND mk.movie_id = mi.movie_id AND k.id = mk.keyword_id AND t.title == 'Austin Powers 4';
"Austin Powers 4",sequel,Germany,""
```

```
postgres=# SELECT title.production_year FROM title where title.title = 'Austin Powers 4';
 production_year 
-----------------
                
(1 row)
```

Oh dear. The production year is null and SQLite thinks null > 2005. Imp and Postgres both think that if there is no production year it can't be > 2005. 

...

Maybe I should test against Postgres instead.

``` julia
function test()
  @test Base.return_types(q1a) == [Relation{Tuple{Vector{String}, Vector{String}, Vector{Int64}}}]
  @test Base.return_types(q2a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q3a) == [Relation{Tuple{Vector{String}}}]
  @test Base.return_types(q4a) == [Relation{Tuple{Vector{String}, Vector{String}}}]
  
  # db = SQLite.DB("../imdb/imdb.sqlite")
  # for q in 1:4
  #   results_imp = eval(Symbol("q$(q)a"))()
  #   query = rstrip(readline("../job/$(q)a.sql"))
  #   query = replace(query, "MIN", "")
  #   frame = SQLite.query(db, query)
  #   num_columns = length(results_imp.columns)
  #   results_sqlite = Relation(tuple((frame[ix].values for ix in 1:num_columns)...), num_columns)
  #   (imp_only, sqlite_only) = Data.diff(results_imp, results_sqlite)
  #   @show q 
  #   @test imp_only.columns == sqlite_only.columns # ie both empty - but @test will print both otherwise
  # end
  
  for q in 1:4
    results_imp = eval(Symbol("q$(q)a"))()
    query = rstrip(readline("../job/$(q)a.sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    query = "copy ($query) to '/tmp/results.csv' with CSV DELIMITER ',';"
    run(`sudo -u postgres psql -c $query`)
    frame = DataFrames.readtable(open("/tmp/results.csv"), header=false, eltypes=[eltype(c) for c in results_imp.columns])
    num_columns = length(results_imp.columns)
    results_pg = Relation(tuple((frame[ix].data for ix in 1:num_columns)...), num_columns)
    (imp_only, pg_only) = Data.diff(results_imp, results_pg)
    @show q 
    @test imp_only.columns == pg_only.columns # ie both empty - but @test will print both otherwise
  end
end
```

All tests pass.

Early bench results (median, in ms, to 2 sf). Imp 0.30 31 82 54. Pg 6.8 530 180 120. SQLite 250 200 93 87.

Ok, what next? Completing all the JOB queries has to come last, because I want to test the number of attempts required to get a good variable ordering by hand. I want to use read-write balanced data-structures in Imp for a fair comparison, instead of the read-optimized arrays I have at the moment, but I don't know if I have time to finish that before the end of the week. One of the steps towards that though is moving from Triejoin to a different worst-case join that makes fewer assumptions about the layout of the data. I'll try it using the same indexes I have now and see if it hurts performance at all.

### 2016 Sep 20

I have a few different index data-structures in mind. Coming up with a interface that can efficiently support joins on any of them is tricky. 

The first thing I want to find out is how much it would cost me to switch from TrieJoin to GenericJoin. I can do that just by rewriting the intersection.

``` julia
body = quote 
  let
    local iter = shortest($var_columns, los, ats, his, $var_ixes)
    local state = start(iter)
    local $var
    while $need_more_results && !done(iter, state)
      ($var, state) = next(iter, state)
      if assign($var_columns, los, ats, his, $var_ixes, $var)
        $body
      end
    end
  end
end
```

Times after are 0.58 47 170 110, times before were 0.30 31 82 54. I'm seeing a lot of allocations, so maybe those subarrays are a problem. Let's try just returning the ixes.

``` julia
body = quote 
  let
    local min_i = shortest(los, ats, his, $var_ixes)
    local column = $var_columns[min_i]
    local at = los[$var_ixes[min_i]]
    local hi = his[$var_ixes[min_i]]
    while $need_more_results && (at < hi)
      let $var = column[at]
        at += 1
        if assign($var_columns, los, ats, his, $var_ixes, $var)
          $body
        end
      end
    end
  end
end
```

(I just want to once again note how much easier my life would be if Julia could stack-allocate things containing points.)

Ok, allocations go away, and the times are a little better - 0.47 42 160 110.

Actually though, in both cases I need to skip repeated values so `at += 1` needs to be `at = gallop(column, $var, at, hi, <=)`, which is a little more expensive in this case.

Altogether it looks like about 2x the time, which makes sense because the `assign` repeats work done by iteration. Maybe if we were a little smarter we could remove that. Let's add a parameter to `assign` that skips whichever column we're drawing from.

``` julia 
function assign(cols, los, ats, his, ixes, value, skip)
  @inbounds begin
    n = length(ixes)
    for c in 1:n
      if c != skip
        ix = ixes[c]
        los[ix+1] = gallop(cols[c], value, los[ix], his[ix], <)
        if los[ix+1] >= his[ix]
          return false
        end
        his[ix+1] = gallop(cols[c], value, los[ix+1], his[ix], <=)
        if los[ix+1] >= his[ix+1]
          return false
        end
      end
    end
    return true
  end
end
```

``` julia
body = quote 
  @inbounds let
    local min_i = shortest(los, ats, his, $var_ixes)
    local ix = $var_ixes[min_i]
    local column = $var_columns[min_i]
    ats[ix] = los[ix]
    while $need_more_results && (ats[ix] < his[ix])
      let $var = column[ats[ix]]
        los[ix+1] = ats[ix]
        ats[ix] = gallop(column, $var, ats[ix], his[ix], <=)
        his[ix+1] = ats[ix]
        if assign($var_columns, los, ats, his, $var_ixes, $var, min_i)
          $body
        end
      end
    end
  end
```

Now we get 0.38 34 73 44. That's actually somewhat better than the original. I'm confused. 

Perhaps lookups in the job queries almost always succeed, because most of them are foreign key joins, so the leapfrog part of triejoin just ends up being wasted work? 

But counting triangles also shows a similar improvement, from 636ms to 598ms. Really not sure what to make of that.

The upside is that it looks like I can switch to GenericJoin without major losses, at least on my current benchmarks.

Julia 0.5 is out for real now, so let's quickly upgrade. Benchmark numbers are about the same.

### 2016 Sep 21

Ok, let's figure out what the query plan should look like. I'm going to preallocate all the needed state up front, to avoid messing around with trying to split up structures so that non-pointerful parts can be stack-allocated.

``` julia
index_1 = index(edge, [1,2])
index_2 = index(edge, [1,2])
index_3 = index(edge, [2,1])

results_a = ...
results_b = ...
results_c = ...

state_ix = @min_by_length(index_1, Val{1}, index_2, Val{1})
while @switch state_ix !done(index_1, Val{1}) !done(index_2, Val{1})
  a = @switch state_ix next!(index_1, Val{1}) next!(index_2, Val{1})
  ok_1 = (state_ix == 1) || skip!(index_1, Val{1})
  ok_2 = (state_ix == 2) || skip!(index_2, Val{1})
  if ok_1 && ok_2
    ...
  end
end
```

I'm using the `Val{1}` to specify the column, making the return type predictable.

I don't really like breaking up the iter interface into done/next. I would rather have a `foreach` and pass a closure, but if `index_1` and `index_2` have different types this risks blowing up the amount of code generated at each step. It seems best to stick to a single path through the query. 

So now I need to implement this iterator interface for sorted arrays. 

``` julia
type Index{T}
  columns::T
  los::Vector{Int64}
  ats::Vector{Int64}
  his::Vector{Int64}
end

function index{T}(relation::Relation{T}, order::Vector{Int64})
  columns = get!(relation.indexes, order) do
    columns = tuple([copy(relation.columns[ix]) for ix in order]...)
    quicksort!(columns)
    columns
  end
  n = length(order) + 1
  los = [1 for _ in 1:n]
  ats = [1 for _ in 1:n]
  his = [length(relation)+1 for _ in 1:n]
  Index(columns, los, ats, his)
end

function span{T,C}(index::Index{T}, ::Type{Val{C}})
  # not technically correct - may be repeated values
  index.his[C] - index.ats[C]
end

function start!{T,C}(index::Index{T}, ::Type{Val{C}})
  index.ats[C] = index.los[C]
end

function next!{T,C}(index::Index{T}, ::Type{Val{C}})
  val = index.columns[C][index.ats[C]]
  index.los[C+1] = index.ats[C]
  index.ats[C] = gallop(index.columns[C], val, index.ats[C], index.his[C], <=)
  index.his[C+1] = index.ats[C]
  val
end

function skip!{T,C}(index::Index{T}, ::Type{Val{C}}, val)
  index.los[C+1] = gallop(index.columns[C], val, index.los[C], index.his[C], <)
  if index.los[C+1] >= index.his[C]
    return false
  end
  index.his[C+1] = gallop(index.columns[C], val, index.los[C+1], index.his[C], <=)
  if index.los[C+1] >= index.his[C+1]
    return false
  end
  return true
end
```

Major debugging follows. I've totally destroyed type inference, somehow.

I learned how to get at closure objects:

``` julia
Base.return_types(eval(Expr(:new, Symbol("#eval_tracks#10"))), (Int64,))
```

I spent the entire day debugging newly created type inference issues and eventually decided to give up on it entirely. It's been nothing but a time sink, and I knew it was going to be a time sink from the beginning and I did it anyway. Note to self - do not ignore that feeling of creeping doom.

Bench numbers for new relation api are 0.45 35 67 38. Within a margin of error of previous numbers.

I belatedly realised that typing this stateful interface was going to be really hard for the other index structures I had in mind, so I also tried out a stateless interface.

``` julia
immutable Finger{C}
  lo::Int64
  hi::Int64
end

function finger(index)
  Finger{1}(1, length(index[1])+1)
end
  
function Base.length{C}(index, finger::Finger{C})
  # not technically correct - may be repeated values
  finger.hi - finger.lo
end

@generated function project{C}(index, finger::Finger{C}, val)
  quote
    column = index[$C]
    down_lo = gallop(column, val, finger.lo, finger.hi, <)
    down_hi = gallop(column, val, down_lo, finger.hi, <=)
    Finger{$(C+1)}(down_lo, down_hi)
  end
end

function Base.start{C}(index, finger::Finger{C})
  finger.lo
end

function Base.done{C}(index, finger::Finger{C}, at)
  at >= finger.hi
end

@generated function Base.next{C}(index, finger::Finger{C}, at)
  quote
    column = index[$C]
    next_at = gallop(column, column[at], at, finger.hi, <=)
    (Finger{$(C+1)}(at, next_at), next_at)
  end
end

function head{C}(index, finger::Finger{C})
  index[C-1][finger.lo]
end
```

`project` and `next` have to be generated because the type inference sees `C+1` as opaque and then terrible things happen down the road.

Times (on battery power, so not super trustworthy) are 0.57 160 79 48. Allocation numbers are really high, especially for q2a. Maybe things are not ending up on the stack as I'd hoped.

Aha, `head` also needs to be generated, again because the type inference sees `C-1` as opaque. Battery power numbers now are 0.35 33 62 35. Happy with that. 

Allocations are still much higher than I expected though. How do I debug this...

Let's take one of the queries and remove clauses until we get to the minimal surprise-allocating query.

``` julia
@query begin
  movie_keyword(_, t_id, _)
  title(t_id, _, _, _, _)
  @when t_id < -1
  return (t_id::Int64,)
end
# 0 results
# (5.06 M allocations: 161.319 MB, 56.56% gc time)
```

The types are all correctly inferred and there is nothing surprising in the lowered code, so let's poke around in the llvm bitcode.

Every gallop is followed by a heap allocation:

``` julia
%159 = call i64 @julia_gallop_73100(%jl_value_t* %148, i64 %158, i64 %at.0, i64 %97) #0
%160 = call %jl_value_t* @jl_gc_pool_alloc(i8* %ptls_i8, i32 1456, i32 32)
```

It's hard to follow the rest of the bitcode, but my first suspicion is that it's allocating the type `Finger{C}`. So let's calculate those at compile time:

``` julia
@generated function project{C}(index, finger::Finger{C}, val)
  quote
    column = index[$C]
    down_lo = gallop(column, val, finger.lo, finger.hi, <)
    down_hi = gallop(column, val, down_lo, finger.hi, <=)
    $(Finger{C+1})(down_lo, down_hi)
  end
end

@generated function Base.next{C}(index, finger::Finger{C}, at)
  quote
    column = index[$C]
    next_at = gallop(column, column[at], at, finger.hi, <=)
    ($(Finger{C+1})(at, next_at), next_at)
  end
end
```

No difference.

Offhand, I notice this chunk of code:

``` julia
SSAValue(16) = (Core.tuple)($(Expr(:new, Data.Finger{2}, :(at), :(next_at@_29))),next_at@_29::Int64)::Tuple{Data.Finger{2},Int64}
SSAValue(5) = SSAValue(16)
SSAValue(28) = (Base.getfield)(SSAValue(5),1)::UNION{DATA.FINGER{2},INT64}
```

Now there is some shitty type inference. It put a thing in a tuple, then took it out again, and immediately forgot what it was.

I wonder if I can get rid of the tuples.

Took a bit of fiddling, but I can use the down_finger as the iterator for the loop too.

``` julia
function finger(index)
  Finger{0}(1, length(index[1])+1)
end
  
function Base.length{C}(index, finger::Finger{C})
  # not technically correct - may be repeated values
  finger.hi - finger.lo
end

@generated function project{C}(index, finger::Finger{C}, val)
  quote
    column = index[$(C+1)]
    down_lo = gallop(column, val, finger.lo, finger.hi, <)
    down_hi = gallop(column, val, down_lo, finger.hi, <=)
    Finger{$(C+1)}(down_lo, down_hi)
  end
end

@generated function Base.start{C}(index, finger::Finger{C})
  quote 
    column = index[$(C+1)]
    hi = gallop(column, column[finger.lo], finger.lo, finger.hi, <=)
    Finger{$(C+1)}(finger.lo, hi)
  end
end

function Base.done{C, C2}(index, finger::Finger{C}, down_finger::Finger{C2})
  down_finger.hi >= finger.hi
end

function Base.next{C,C2}(index, finger::Finger{C}, down_finger::Finger{C2})
  column = index[C2]
  hi = gallop(column, column[down_finger.hi], down_finger.hi, finger.hi, <=)
  Finger{C2}(down_finger.hi, hi)
end

function head{C}(index, finger::Finger{C})
  index[C][finger.lo]
end
```

Still have 5m allocations though. 

``` julia
function foo()
  finger = Finger{1}(1,1)
  for _ in 1:1000
    finger = Finger{1}(finger.hi + finger.lo, finger.lo)
  end
  finger
end
```

This doesn't allocate at all. So fingers definitely *can* be stack-allocated. The lowered code doesn't look any different to me. Not sure what else to do. 

Inlining all the finger functions has no effect.

Changing the switch macros to avoid accidental returns actually increases the number of allocations to 7m. That's interesting. It shouldn't do anything...

### 2016 Sep 22

Yesterday was a bit long, so I want to just try a few simple things today and switch project for the rest of the day.

Unlike the type system, there's no insight into how Julia decides whether or not to stack-allocate something, so I've just been repeatedly guessing. I could really do with a tool that explains the decision. I sent a question to the mailing list, asking if there is a better debugging process than just trying things at random.

Judging by the order of occurence, it looks like the allocations are for `start` and `project` but not for `next`. 

Poking around inside the llvm bitcode I'm about 50% confident that the allocation for `next` is being reusued, but it's still not on the stack. But while I'm in there I notice that there are a lot of checks for undefined variables. 

Of course - if the compiler can't prove that a variable might be null, it can't possibly stack-allocate it. So what if I just zero out the fingers at the beginning:

``` julia
$([:(local $down_finger = Finger{$col_ix}(0,0)) for (down_finger, col_ix) in zip(down_fingers, col_ixes)]...)
```

Down to 2.5m allocations. Excellent. That also explains why messing with `switch` affected the number of allocations - it made it harder for the compiler to figure out that the variable was definitely allocated.

Solving the same problem for `var` reduces the allocations to 32. Happy days.

Bench numbers 0.33 28 60 34. 

Let's comb through the llvm bitcode a little more to make sure there are no other surprises, and then I'll move on.

There's some messy allocation and generic calls resulting from the fact that the return type of `index` is not inferrable. I don't see a way around this that also leaves the return type of `head` inferrable. The latter is much more important, so this stays for now. It will only matter in subqueries, and I have other possible solutions for those.

I will move it into the index function itself though, so that other data-structures can have their own individual hacks.

``` julia
@generated function index{T, O}(relation::Relation{T}, ::Type{Val{O}})
  order = collect(O)
  typs = [:(typeof(relation.columns[$ix])) for ix in order]
  quote
    index(relation, $order)::Tuple{$(typs...)}
  end
end
```

I guess the next thing to do is build a new index type. I have a bunch of ideas, but let's start tomorrow with something really simple - nested hashtables.

### 2016 Sep 23

I wrote most of the nested hashtables implementation before getting bogged down in details of the finger protocol again. I ended up going back to something similar to my original mutable implementation, except with individual fingers rather than some hard-to-type agglomeration of state. While the actual instructions executed are probably the same, it feels much easier to reason about.

``` julia
type Finger{T}
  column::Vector{T}
  lo::Int64
  hi::Int64
end

function finger(relation::Relation, index)
  Finger(Void[], 1, length(index[1])+1)
end

@inline function finger(relation::Relation, index, finger, col_ix)
  Finger(index[col_ix-1], 0, 0)
end
  
function Base.length(finger::Finger)
  # not technically correct - may be repeated values
  finger.hi - finger.lo
end

function project{T,T2}(finger::Finger{T}, down_finger::Finger{T2}, val)
  down_finger.lo = gallop(down_finger.column, val, finger.lo, finger.hi, <)
  down_finger.hi = gallop(down_finger.column, val, down_finger.lo, finger.hi, <=)
  down_finger.lo < down_finger.hi
end

function Base.start{T,T2}(finger::Finger{T}, down_finger::Finger{T2})
  down_finger.lo = finger.lo
  down_finger.hi = gallop(down_finger.column, down_finger.column[down_finger.lo], down_finger.lo, finger.hi, <=)
  down_finger.lo < down_finger.hi
end

function Base.next{T,T2}(finger::Finger{T}, down_finger::Finger{T2})
  if down_finger.hi >= finger.hi
    false
  else
    down_finger.lo = down_finger.hi
    down_finger.hi = gallop(down_finger.column, down_finger.column[down_finger.lo], down_finger.lo, finger.hi, <=)
    true
  end
end

function head{C, C2}(finger::Finger{C}, down_finger::Finger{C2})
  down_finger.column[down_finger.lo]
end
```

``` julia
starts = [:(start($finger, $down_finger)) for (finger, down_finger) in fingers]
projects = [:((ix == $ix) || project($finger, $down_finger, $(esc(var)))) for (ix, (finger, down_finger)) in enumerate(fingers)]
heads = [:(head($finger, $down_finger)) for (finger, down_finger) in fingers]
nexts = [:(next($finger, $down_finger)) for (finger, down_finger) in fingers]
body = quote 
  let 
    local ix = @min_by_length($(fingers...))
    local more = @switch ix $(starts...)
    local $(esc(var))
    while $need_more_results && more
      $(esc(var)) = @switch ix $(heads...)
      if $(reduce((a,b) -> :($a && $b), projects))
        $body
      end
      more = @switch ix $(nexts...)
    end
  end
end
```

Times now are 0.35 31 68 40. The slight slowdown didn't occur from the move to the mutable api, but only after I moved the columns into the individual fingers. Best guess is the extra field access is a little more expensive than fetching the column out of a register? I'll live, I guess.

I finished the core of the nested relations implementation, but it's currently missing results on some queries. I'll finish debugging tomorrow.

### 2016 Sep 24

Feeling a little unmotivated today so I'm working on fixing minor annoyances in my environment. 

I fixed palm detection on my touchpad, disabled the unity multi-touch gestures that I keep using by accident, and in the process accidentally fixed a race condition where some of my window manager keyboard shortcuts would get swallowed. 

``` bash
jamie@machine:~$ sudo cat /etc/modprobe.d/synaptics.conf 
blacklist i2c-designware-platform    
jamie@machine:~$ tail -n 2 .bashrc
pkill syndaemon
syndaemon -i 0.2 -K -d
```

I disabled coasting to avoid the notorious ctrl-zoom bug.

``` bash
jamie@machine:~$ tail -n 1 .bashrc
synclient CoastingSpeed=0
```

I moved all of my Julia and Atom packages off master and onto stable versions, now that Julia 0.5 is officially released. I also updated Atom by hand, since the automatic update fails.

The default Atom spell checker does not work if your system locale is not en-US. I switched to the spell-check-test package which allows specifying alternate locales.

If I have one atom window open and I accidentally close it, restarting atom will restore that window. If have two atom windows open and I accidentally close one, it's gone forever. The project-manager package is a decent workaround for this.

I've had problems with graphical corruption in Atom. The internet seems to believe that SNA is the most likely culprit, so I'm tentatively switching to UXA and I'll see if it happens again in the next few days.

``` bash
jamie@machine:~$ sudo cat /etc/X11/xorg.conf
Section "Device"
	Identifier "Intel Graphics"
	Driver "intel"
	Option "AccelMethod" "uxa"
EndSection
```

### 2016 Sep 25

Graph queries for nested hashes are missing a result. Let's try debugging again now that I'm fresh.

The finger implementation looks correct.

The indexes for the graph are correct.

Dumping the assigned vars:

``` julia
a = 4
b = 2
a = 2
b = 2
a = 3
b = 2
a = 1
b = 2
c = 3
```

All the possible values of `a` are checked. It also checks `a=2, b=2` and `a=3, b=2` which are not possible combinations. In fact, `b=2` every time.

Let's also dump the fingers:

``` julia
(#542#finger_1_0,#543#finger_1_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(2=>nothing),2=>Dict(3=>nothing),3=>Dict(4=>nothing,1=>nothing),1=>Dict(2=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict{Int64,Void}(),-1))
(#550#finger_5_0,#551#finger_5_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(3=>nothing),2=>Dict(4=>nothing,1=>nothing),3=>Dict(2=>nothing),1=>Dict(3=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict{Int64,Void}(),-1))
a = 4
(#543#finger_1_1,#544#finger_1_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(2=>nothing),2),Nested.Finger{Void}(nothing,-1))
(#546#finger_3_0,#547#finger_3_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(2=>nothing),2=>Dict(3=>nothing),3=>Dict(4=>nothing,1=>nothing),1=>Dict(2=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict{Int64,Void}(),-1))
b = 2
a = 2
(#543#finger_1_1,#544#finger_1_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(2=>nothing),6),Nested.Finger{Void}(nothing,17))
(#546#finger_3_0,#547#finger_3_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(2=>nothing),2=>Dict(3=>nothing),3=>Dict(4=>nothing,1=>nothing),1=>Dict(2=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict(3=>nothing),-1))
b = 2
a = 3
(#543#finger_1_1,#544#finger_1_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(2=>nothing),7),Nested.Finger{Void}(nothing,17))
(#546#finger_3_0,#547#finger_3_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(2=>nothing),2=>Dict(3=>nothing),3=>Dict(4=>nothing,1=>nothing),1=>Dict(2=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict(3=>nothing),-1))
b = 2
a = 1
(#543#finger_1_1,#544#finger_1_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(2=>nothing),16),Nested.Finger{Void}(nothing,17))
(#546#finger_3_0,#547#finger_3_1) = (Nested.Finger{Dict{Int64,Dict{Int64,Void}}}(Dict(4=>Dict(2=>nothing),2=>Dict(3=>nothing),3=>Dict(4=>nothing,1=>nothing),1=>Dict(2=>nothing)),-1),Nested.Finger{Dict{Int64,Void}}(Dict(3=>nothing),-1))
b = 2
(#547#finger_3_1,#548#finger_3_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(3=>nothing),-1),Nested.Finger{Void}(nothing,-1))
(#551#finger_5_1,#552#finger_5_2) = (Nested.Finger{Dict{Int64,Void}}(Dict(3=>nothing),-1),Nested.Finger{Void}(nothing,-1))
c = 3
```

On the second line, it finds `b=2` from one of the fingers and then projects in the other and fails. This is incorrect, both have a key for `b=2`.

Ah, the project doesn't fail, it hits `a<b` on the next line. I forgot about those.

Next we go to `a=2` which is legit, but then somehow hit `b=2` again. 

Aha - `next` needs to set `down_finger.index` as well as `down_finger.state`. Oops.

This query works now. The test doesn't, because it looks at internal state. I need to add an method that dumps out the columns for tests.

I'll have to remove the return type tests entirely, but they aren't all that useful now that I've given up on inference anyway.

Nested hashtables now work ok for simple queries, but they are far too memory inefficient to load the JOB data, even with 32GB of RAM. But that's ok, they weren't intended to be the final solution and they helped me hash out the finger protocol.

### 2016 Sep 28

Totally forgot to diarize today.

I made a Franken-hashtable today that uses the sorted columns as backing storage, and builds a hashtable of ranges pointing into each column. 

``` julia
type Index{C}
  columns::C
  hashtables::Vector{Vector{UnitRange{Int}}}
  probe_ranges::Vector{UnitRange{Int}}
end
```

I'm using Robin Hood hashing, mostly because it's easy to implement. The theory says that the maximum probe distance at 90% occupancy should be around 6. I'm seeing ~50. Some quick experiments confirm that some buckets are seeing ~20 collisions, so a probe distance of 6 is out of the question with this hash function. I'll stick to 66% occupancy instead.

Each finger keeps track of the last hash so the down finger only has to combine that with it's new key, rather than rehashing the whole row.

I'm able to get the JOB data loaded, although JLD doesn't play nicely with the UnitRanges in the hashtables so saving/loading is a mess.

Max probe lengths on the JOB tables hover around 20.

Benchmark times are poor. As usual there are a ton of allocations that need to be tracked down.

Peeking at the code for a simple query, it looks like the types in the indexes aren't known. Simple fix.

Times are still poor. Let's have the hashtable lookup bail out early on empty slots - forgot to include that earlier.

Numbers are still not great. 0.64 110 190 140. 

### 2016 Sep 29

I still sometimes run out of memory when trying to run the benchmarks with hashtables. Which is ridiculous. It's a 6GB dataset on disk but the columns alone are taking up 20GB in memory. I've been putting off dealing with that for a while, but it looks like it's time.

Julia can measure memory allocation by line, but that's just going to tell me that the allocation all comes from loading the huge datasets into memory. 

Let's just load a single file as one string and see what happens:

``` bash
jamie@machine:~/imp$ du -h ../imdb/movie_info.csv 
920M	../imdb/movie_info.csv
```

``` julia
@time begin 
  s = readstring(open("../imdb/movie_info.csv"))
  nothing
end
# 0.223532 seconds (150 allocations: 919.144 MB, 6.54% gc time)
```

(The weird construction is so that the Atom plugin doesn't try to render the whole string. It really needs some way to lazily render large data-structures.)

``` bash
PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND     
842 jamie     20   0 10.485g 1.947g  63168 R  99.7  6.2   0:37.76 julia 
```

``` julia
@time gc()
# 0.132644 seconds (648 allocations: 33.249 KB, 98.78% gc time)
```

The gc reports taking 0.13s. It takes about 10s to show up in Atom, and then julia-client crashes. [Reliably](https://github.com/JunoLab/atom-julia-client/issues/247).

But julia-client aside, just reading in the whole file results in a reasonable amount of allocation. What about parsing the csv using DataFrames.jl?

``` julia
job = @time read_job("movie_info")
# 52.275075 seconds (348.80 M allocations: 10.628 GB, 6.00% gc time)
```

``` bash
PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND  
2652 jamie     20   0 11.562g 5.583g  13140 S   0.0 17.9   1:12.23 julia 
```

``` julia 
gc()
```

``` bash
PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND
2652 jamie     20   0 9896544 3.560g  13732 S   0.0 11.4   1:14.83 julia  
```

So ~3.5gb just to split it into strings. That seems wrong.

``` julia
length(Set(job.columns[4].data)) # 2720930
length(Set(job.columns[5].data)) # 133610
sum([1 for s in job.columns[4].data if s == ""]) # 0
sum([1 for s in job.columns[5].data if s == ""]) # 13398750
```

Are empty strings interned in Julia?

``` julia
pointer_from_objref("") == pointer_from_objref("") # false
```

Nope.

``` julia
ss = Dict{String, String}()
job.columns[4].data = String[get!(ss, s, s) for s in job.columns[4].data]
job.columns[5].data = String[get!(ss, s, s) for s in job.columns[5].data]
gc()
```

This is taking forever...

Drops memory usage to 2.8gb. Better than nothing. Stills seems like a lot of overhead for a <1gb file.

Small numbers are only a few bytes in ascii, but always 8 bytes as an Int64. Postgres treats `integer` as 32 bits. SQLite stores it adaptively depending on the max value. Let's copy SQLite.

The first three columns of this table end up being Int32, Int32, Int8. Only saves about 140mb, but at least the increased locality might help when joining.

It seems like the remaining overhead must be entirely from breaking up into many tiny string allocations. Can't fix that easily unless Julia unifies String and SubString at some point in the future.

Interning probably won't survive load/save with JLD, so I might have to re-intern on loading...

Anyway, back to hashtables. I was trying to figure out why they are so slow. The Julia profiler is not particularly helpful - most of the trace is in Hashed.get_range as expected, but it also places a lot of time in Relation. But timing each part of the query with @time disagrees, and I trust the latter more.

I suspect the memory overhead of the current hash-table representation is a problem. I can test that by changing UnitRange{Int64} to UnitRange{Int32} and seeing how it affects the timings.

Slightly better: 0.56 83 160 130. Not enough to matter. No more hashtables.

Constantly loading this dataset is a huge time-sink, so let's see if I can make it faster. I have an idea that DataFrames is loading stuff row-wise, which means dynamic dispatch on types (unless it's doing something clever with generated types). Let's try doing stuff column-wise instead. I'll use sqlite to avoid having to do any csv parsing.

``` julia
function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict()
  table_column_types = Dict()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), column_type)
  end
  relations = Dict()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      columns = []
      for (column_name, column_type) in zip(column_names, column_types)
        query = "select $column_name from $table_name"
        lines = readlines(`sqlite3 ../imdb/imdb.sqlite $query`)
        if column_type == "integer"
          numbers = Int64[line == "" ? 0 : parse(Int64, line) for line in lines]
          minval, maxval = minimum(numbers), maximum(numbers)
          typ = first(typ for typ in [Int8, Int16, Int32, Int64] if (minval > typemin(typ)) && (maxval < typemax(typ)))
          column = convert(Vector{typ}, numbers)
        else 
          interned = Dict{String, String}()
          column = String[get!(interned, line, line) for line in lines]
        end
        push!(columns, column)
      end
      relations[table_name] = Relation(tuple(columns...), 1)
    end
  end
  relations
end
```

A few back-and-forths with code_warntype refine this to:

``` julia
function read_job()
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict{String, Vector{String}}()
  table_column_types = Dict{String, Vector{String}}()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), column_type)
  end
  relations = Dict{String, Relation}()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      columns = Vector[]
      for (column_name, column_type) in zip(column_names, column_types)
        query = "select $column_name from $table_name"
        lines::Vector{String} = readlines(`sqlite3 ../imdb/imdb.sqlite $query`)
        if column_type == "integer"
          numbers = Int64[(line == "" || line == "\n") ? 0 : parse(Int64, line) for line in lines]
          minval = minimum(numbers)
          maxval = maximum(numbers)
          typ = first(typ for typ in [Int8, Int16, Int32, Int64] if (minval > typemin(typ)) && (maxval < typemax(typ)))
          push!(columns, convert(Vector{typ}, numbers))
        else 
          interned = Dict{String, String}()
          for ix in 1:length(lines)
            lines[ix] = get!(interned, lines[ix], lines[ix])
          end
          push!(columns, lines)
        end
      end
      relations[table_name] = Relation(tuple(columns...), 1)
    end
  end
  relations
end
```

Got to go now, will see how long it takes tomorrow.

### 2016 Sep 30

The load time is cut in half. Still much too long. I'll try saving just the columns, so at least I don't have to rebuild whenever I change the representation of relations.

Let's try using substrings to reduce allocation.

``` julia
function read_job()
  db = SQLite.DB("../imdb/imdb.sqlite")
  schema = readdlm(open("data/job_schema.csv"), ',', header=false, quotes=true, comments=false)
  table_column_names = Dict{String, Vector{String}}()
  table_column_types = Dict{String, Vector{String}}()
  for column in 1:size(schema)[1]
    table_name, ix, column_name, column_type = schema[column, 1:4]
    push!(get!(table_column_names, table_name, []), column_name)
    push!(get!(table_column_types, table_name, []), column_type)
  end
  tables = Dict{String, Tuple}()
  for (table_name, column_names) in table_column_names
    if isfile("../imdb/$(table_name).csv")
      column_types = table_column_types[table_name]
      @show table_name column_names column_types
      columns = Vector[]
      for (column_name, column_type) in zip(column_names, column_types)
        query = "select $column_name from $table_name"
        lines::Vector{SubString{String}} = split(readstring(`sqlite3 ../imdb/imdb.sqlite $query`), '\n')
        if column_type == "integer"
          numbers = Int64[(line == "") ? 0 : parse(Int64, line) for line in lines]
          minval = minimum(numbers)
          maxval = maximum(numbers)
          typ = first(typ for typ in [Int8, Int16, Int32, Int64] if (minval > typemin(typ)) && (maxval < typemax(typ)))
          push!(columns, convert(Vector{typ}, numbers))
        else 
          push!(columns, lines)
        end
      end
      tables[table_name] = tuple(columns...)
    end
  end
  tables
end
```

Still a ton of allocation. Of course, because SubString contains a pointer so it can't be stack-allocated. It's also immutable, so I can't just allocate one and mutate it in a loop. Worst of both worlds.

This is really frustrating. I'm taking one big string and turning it into one big chunk of integers, and there is no way to use the string api to do this without creating a new heap object for every line.

It looks like the [fix](https://github.com/JuliaLang/julia/pull/18632) for this is pretty close to landing. Hopefully in the next month or two I get to stop complaining about this. Maybe I should just finish the JOB benchmarks with the code as it stands, and then come back to performance work later once it's less frustrating.

For some reason JLD is now hanging when I try to save the columns. I tried Feather. which [crashed](https://github.com/JuliaStats/Feather.jl/issues/21). I tried HDF5 but now it's hanging before I even get to saving. Is my SSD dying? So frustrating.

<break>

I've spent 10 days now working on indexes. It feels like it's been a meandering and aimless slog. I think the reason for this is that I don't have a concrete problem to solve. I only wanted indexes at this point for a 'fair' comparison with sqlite so that I can write a progress report without feeling dishonest. 

Engineering is mostly about tradeoffs. To decide which tradeoffs to make I need concrete use-cases against which to evaluate them and a good-enough point so I know when to stop working. The most common mistake I make in programming is to try and build something without those. I end up constantly changing my mind, flailing back and forth between different designs. It's stressful and exhausting. 

So no more indexes. Let's do the minimum possible to finish:

* Remove Hashed indexes
* Return to single indexes per column
* Write all benchmarks
* Benchmark insert time
* Measure import and load time 
* Write report
* Write benchmark repro
* Fix minesweeper
* Add readme

Hashed indexes are gone.

Bunch of faff with serialization - most Julia serialization libraries I tried don't work. Still have horrible load times. But I have the original dataframes saved and back to single indexes.

Bench times for single indexes: 3.04 58.5 99.5 52.5. Looking all the way back, I previously had 1.66 68.8 116 46 for single indexes. 

I added the first 26 of 116 queries. Very time-consuming work.

Benchmarks so far:

```
1a imp=3.053742 sqlite=272.002707 pg=8.245
1b imp=0.025493 sqlite=270.378863 pg=0.3545
1c imp=0.3997515 sqlite=269.570717 pg=6.4695
1d imp=0.0296175 sqlite=268.658936 pg=0.409
2a imp=52.3988785 sqlite=248.268076 pg=668.3215
2b imp=50.513265 sqlite=244.603772 pg=637.964
2c imp=46.459809 sqlite=247.278891 pg=609.987
2d imp=67.589642 sqlite=248.307465 pg=829.885
3a imp=103.3306 sqlite=108.787906 pg=200.249
3b imp=55.1109555 sqlite=54.162339 pg=131.461
3c imp=145.452778 sqlite=239.3878935 pg=370.3905
4a imp=52.49697 sqlite=1007.083345 pg=134.812
4b imp=51.2409345 sqlite=115.5015855 pg=72.392
4c imp=59.6302305 sqlite=2306.46438 pg=146.917
5a imp=97.826341 sqlite=498.090845 pg=201.365
5b imp=94.930138 sqlite=498.81496 pg=197.129
5c imp=106.009394 sqlite=586.696994 pg=258.47
6a imp=2.037003 sqlite=15.344694 pg=18.965
6b imp=1153.088708 sqlite=90.9772715 pg=273.2355
6c imp=0.0359965 sqlite=11.6395965 pg=14.14
6d imp=1137.420032 sqlite=2259.020631 pg=7490.549
6e imp=2.098454 sqlite=15.563986 pg=21.8
6f imp=3175.456101 sqlite=2186.361888 pg=7876.016
7a imp=3.101584 sqlite=737.849858 pg=1095.4915
7b imp=2.202317 sqlite=122.181171 pg=291.897
7c imp=597.880991 sqlite=8784.628611 pg=3715.3475
```

In 6b and 6d I do a table scan with a regex. Apparently a bad idea. In 6f I'm suffering from not having factorized queries - a lot of those movies will be repeatedly visited. For all three I would probably be better off starting at title and taking advantage of the early bailout. I'm not going to change any queries until I have the first-attempt results for all of them though, since what I want to demonstrate in this report is that query planning by hand is feasible.

### 2016 Oct 1

Up to q14 now. Tedious.

### 2016 Oct 3

Finished! Did I mention tedious?

### 2016 Oct 4

First benchmark run.

```
query_name imp pg sqlite
1a 3.071881 6.7705 279.363504
1b 0.0263035 0.299 274.663133
1c 0.4211085 5.0635 275.692298
1d 0.033006 0.3205 275.171367
2a 55.495408 561.2585 214.3062825
2b 55.160391 546.6990000000001 213.441694
2c 49.817386 517.1514999999999 212.778674
2d 66.310482 725.064 217.177619
3a 92.679652 180.15 96.235637
3b 48.3193085 117.828 48.850807
3c 131.290474 328.961 219.033419
4a 46.43083 117.355 980.9101165
4b 44.559459 61.4545 104.938573
4c 50.2855675 129.01999999999998 2366.23596
5a 97.617968 200.446 525.961315
5b 97.432777 200.458 520.4906375
5c 107.53948 267.85 603.459389
6a 1.85895 17.997999999999998 13.362409
6b 1028.928198 249.967 85.233788
6c 0.038648 11.541 11.333578
6d 1030.35003 6617.195 2084.073462
6e 1.8559195 18.212 14.013924
6f 2943.7297565 6895.882 2141.013491
7a 2.958819 976.3245 686.1519205
7b 2.0541875 253.956 108.449734
7c 554.682386 3375.3125 7810.208279
8a 37.477649 3003.2889999999998 2128.652536
8b 35.471968 390.313 134.5666935
8c 3397.110242 8374.778 52601.765993
8d 374.441778 4697.5655 47468.901567
9a 1001.729029 640.039 8210.799575
9b 538.684329 630.583 3177.2906035
9c 1024.892182 1843.491 46727.984953
9d 1499.7894565 5042.235 47155.854412
10a 345.042314 561.2235000000001 380.0974195
10b 97.9970765 211.739 129.301383
10c 14963.198416 7037.848 29738.480588
11a 7.1610035 57.05 4.075278
11b 3.2977385 29.478 0.838232
11c 35.5103445 206.652 739.169877
11d 35.8314435 179.1395 2715.0278505
12a 201.6289805 381.6865 2070.368664
12b 0.351489 0.663 1918.953183
12c 278.645284 1433.67 3084.5736475
13a 663.082294 3014.3655 744.67811
13b 1066.653256 1123.806 1780.47162
13c 1053.354259 1088.156 1697.40457
13d 3238.985109 5006.748 5474.839887
14a 84.0854095 370.95500000000004 512.4031535
14b 23.56192 153.8 282.514509
14c 222.752032 807.959 1530.3825565
15a 544.531318 702.47 7832.009614
15b 2.965658 32.719 37.654282
15c 3858.9454415 667.2735 394152.840059
15d 3636.7637605 784.207 1.178934880293e6
16a 13.103604 232.153 56.2300195
16b 8518.017965 20322.864 10938.242331
16c 573.129466 1677.619 863.188182
16d 448.0165805 1342.874 674.621042
17a 1809.938807 13111.624 7526.848849
17b 1713.686166 8762.209 13585.672607
17c 1702.306961 8492.641 13442.318623
17d 1738.433212 8589.439 13857.123371
17e 5358.413131 13629.412 6309.469217
17f 1864.826318 11532.09 13757.17439
18a 810.201093 10065.163 21057.091939
18b 87.452809 339.207 529.5813095
19a 629.3732875 652.357 5555.695429
19b 493.187239 355.527 1058.993306
19c 660.457585 1917.405 25442.382734
19d 1033.585309 15797.603 30571.114081
20a 711.6771485 3251.322 107023.383618
20b 271.369411 2301.563 9355.733106
20c 222.872613 1267.932 2127.687286
21a 12.462066 77.983 4.054863
21b 11.3896365 60.558 3.594502
21c 12.963332 68.1635 5.8681245
22a 308.396646 518.366 977.3640315
22b 85.853997 325.516 670.5025745
22c 1159.071359 2449.2664999999997 3379.20423
22d 2126.412915 1110.95 6101.439595
23a 265.550127 380.778 98362.413036
23b 17.536473 42.723 1438.3061185
23c 701.8458905 454.929 224723.583161
24a 434.0135775 386.512 7449.437054
24b 1.66058 39.0105 33.206921
25a 1044.914869 3649.65 23759.102137
25b 31.157697 611.747 436.175846
25c 7710.527347 10181.595 24783.495814
26a 97.623228 1157.52 1324.76143
26b 19.634994 265.987 529.391853
26c 228.226249 2130.604 2117.88685
27a 8.718032 25.88 11.726329
27b 3.509686 17.653 5.6765365
27c 8.689416 26.7815 15.320234
28a 278.040839 4366.493 1959.46839
28b 133.448594 346.54150000000004 1063.917472
28c 168.1971225 1008.5464999999999 2624.989602
29a 0.2306125 128.668 187.388822
29b 0.232277 17.816 183.5762085
29c 19.112286 645.936 1214.135009
30a 757.11687 5787.802 821.827739
30b 84.217824 796.575 168.963133
30c 4335.9167925 3540.66 2898.6302355
31a 432.1715465 2843.2295 2367.575553
31b 96.161288 529.9435 432.2645775
31c 692.5884955 2835.7780000000002 2394.80942
32a 0.00469 11.434 237.68343
32b 19.072634 146.0395 234.221476
33a 57.208684 24.683 5.2306125
33b 74.584004 52.11 2.483724
33c 779.229866 31.918 51.4210295
```

Looking at the cases where Imp wins dramatically, they all seem to be single key lookups on indexes that pg and sqlite don't have. That seems unfair, so let's index every column in both databases.

``` julia
for (table_name, column_names) in table_column_names
  for column_name in column_names
    println("create index index_$(table_name)_$(index_name) on $(table_name)($(index_name));")
  end
end
```

I'm going to rebuild the Julia sysimg while I'm at it. Not sure if it will make much of a difference, but it's worth doing.

``` julia
include(joinpath(dirname(JULIA_HOME),"share","julia","build_sysimg.jl")); build_sysimg(force=true)
```

While both of those are running, let's look through the queries that do poorly and figure out what I'm doing wrong.

6b. 12x slower. Table scan with regex.

6f. 1.4x slower. Unclear. Repeatedly hitting the same title?

9a. 1.5x slower. Unclear. No clear starting point. Perhaps put production_year earlier?

10c. 2.1x slower. Unclear.

11a. 1.7x slower. Unclear. Maybe scan company name table?

11b. 3.9x slower. Unclear. Maybe scan company name table?

15c. 5.8x slower. Unclear. 

15d. 4.6x slower. Unclear.

19b. 1.4x slower. Maybe title and name filters are too far down?

21a-c. ~3x slower. Company name probably should be higher.

22d. 1.9x slower. Unclear.

23c. 1.5x slower. Internet release too far down?

24a. 1.1x slower. 

30c. 1.5x slower. Probably should have started from keywords, not genres.

33a-c. ~30x slower. Not sure about a and c, but b should probably have raised the production_year much higher. Maybe a and c should have used rating?

The most surprising thing for me is the variance between pg and sqlite. Pg is between 3000x faster and 35x slower than sqlite depending on the query.

When there is a clear starting point, the Imp plan is usually obvious and it does well. The really bad numbers come out when there are a bunch of non-indexed constraints and I have to choose which table to scan, without even having looked at how big the tables are.

Imp would definitely benefit from having range lookups (eg 2000 <= production_year <= 2010) which I could already implement pretty well. Regex lookups would be similarly helpful, but probably too much effort to implement right now.

I have a suspicion that factorized queries would do better. In a lot of the query plans I was having to choose which joins to repeatedly evaluate, when a factorized query wouldn't have to repeat any of them. There are also places where I suspect that I'm repeatedly visiting the same keys.

Rerunning the benchmarks now with the extra indexes. I'm expecting to see pg and sqlite to overtake Imp. 

### 2016 Oct 5

```
1a 3.1202115 6.9595 714.755314
1b 0.0257545 0.266 1253.4464195
1c 0.4013495 5.2855 596.911987
1d 0.0310445 0.2915 1549.806406
2a 57.964222 528.5129999999999 226.979648
2b 57.0518545 514.4715 226.992055
2c 50.192977 492.427 224.74445
2d 73.451781 683.819 231.8685845
3a 103.396302 176.97500000000002 362.172093
3b 55.3837195 115.32 5.396267
3c 142.2892345 323.4915 2926.0697205
4a 53.6762765 114.16 1032.958873
4b 50.9984105 14.268 174.5805495
4c 59.424809 125.87 2312.750792
5a 95.082645 201.503 509.9380735
5b 97.3775635 202.708 510.5966295
5c 111.208154 251.058 594.544603
6a 1.957761 5.06 2.6287995
6b 1006.102863 217.701 64.005839
6c 0.031437 0.505 0.277817
6d 1000.947793 6468.033 2320.37252
6e 1.830566 7.081 3.063485
6f 2934.395963 6784.543 2358.775037
7a 2.8209975 6.5225 4.782431
7b 1.960479 2.05 0.800264
7c 552.2459225 3381.3705 2528.5973425
8a 35.107938 2988.8050000000003 58.6299025
8b 34.801288 190.562 97.5034705
8c 3574.727053 8322.726 15994.57942
8d 392.911171 4598.378000000001 2152.827988
9a 975.9131825 296.693 3814.6485415
9b 506.1561515 287.195 586.138012
9c 989.774157 1842.02 4187.451232
9d 1506.518 4976.998 5004.8475245
10a 343.275878 536.877 5716.062919
10b 95.312548 183.611 5484.824958
10c 14496.398719 6897.331 28601.112296
11a 6.811867 38.295 450.2559045
11b 3.238811 20.11 452.9634425
11c 34.8917255 172.79399999999998 1817.943096
11d 35.194864 160.855 21247.530559
12a 197.6314385 300.378 1567.7824555
12b 0.34365 0.68 1672.541799
12c 273.457263 1383.03 2382.885773
13a 659.979154 2933.4440000000004 1779.096776
13b 1049.678017 1114.0035 1079.116244
13c 1029.872256 1072.366 953.9510605
13d 3073.325846 4858.845499999999 4603.9182085
14a 81.2734675 343.55 508.1616175
14b 23.130869 136.6515 273.506766
14c 216.014408 746.6220000000001 1609.584826
15a 540.610489 701.432 7930.997191
15b 2.782742 3.563 1168.457507
15c 3700.361472 667.654 1.169465944449e6
15d 3519.1675725 787.214 1.198311287141e6
16a 12.5486395 209.297 47.10287
16b 8540.832453 19933.58 10504.133574
16c 579.138577 1629.88 806.040774
16d 440.858181 1305.267 620.568618
17a 1791.891394 12908.545 5364.176037
17b 1691.674157 8691.878 11440.365504
17c 1680.976286 8355.929 11418.454143
17d 1717.838847 8461.689 11650.675027
17e 5248.298737 13241.499 5375.40818
17f 1821.900949 11355.119 11748.35144
18a 818.59906 10105.076 1574.11824
18b 96.975638 259.153 0.2503385
19a 604.091671 298.06 3881.0141455
19b 470.478289 210.752 746.163688
19c 639.070404 1675.016 4969.1130775
19d 988.719035 15530.924 10587.216846
20a 697.9063015 3183.5555 3589.6914605
20b 295.4850945 2228.325 900.5101145
20c 221.0699405 1211.642 2063.360925
21a 12.228561 47.725 485.073355
21b 11.24698 43.069 481.065692
21c 12.712685 48.596 483.426167
22a 324.2410095 468.607 1059.349457
22b 97.917814 300.765 715.962638
22c 1102.837342 2144.779 6873.302364
22d 2157.846096 1038.7235 14583.581357
23a 259.119604 381.3295 96696.354517
23b 16.7828095 27.257 765.868064
23c 667.43128 451.844 227790.97049
24a 510.320524 309.669 3996.475217
24b 1.64739 3.75 3952.9654885
25a 1044.864274 3597.538 1486.341207
25b 30.467337 378.8765 41.4811205
25c 8029.421799 10246.668 5307.931861
26a 105.9152885 1906.107 1304.715799
26b 20.623313 48.375 523.0151345
26c 247.196486 1733.723 2118.861378
27a 8.936741 45.89 478.353622
27b 3.5890685 44.2795 477.592084
27c 8.8660915 82.846 481.728975
28a 287.8502915 1017.9929999999999 1386.401539
28b 137.398647 903.4725 521.4545765
28c 174.171348 355.126 2206.824854
29a 0.2221165 492.3995 3920.3215215
29b 0.227203 21.439 3966.124948
29c 19.9306195 631.2484999999999 5179.043264
30a 815.20509 5289.308 565.059446
30b 86.6437595 509.36400000000003 32.753604
30c 4705.838128 3557.5755 2714.830581
31a 460.270231 2697.607 607.882617
31b 100.4009585 493.0725 106.1555815
31c 744.030692 2713.9759999999997 5114.846874
32a 0.004895 0.105 0.162307
32b 20.813718 132.73399999999998 55.7848295
33a 52.25795 23.877 49.8586
33b 79.896146 23.746499999999997 62.552559
33c 741.256199 50.125 822.530545
```

Both postgres and sqlite do worse on some queries if you give them the option of using more indexes. I think I knew this theoretically but I'm still really shocked to see it happen so easily. 

SQLite also does really well on some of these queries, but when I look into individual examples I find that it does so by returning the wrong answers. That's not ideal.

``` julia
function test_sqlite(qs = query_names())
  db = SQLite.DB("../imdb/imdb.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  for query_name in qs
    results_imp = eval(Symbol("q$(query_name)"))()
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    frame = SQLite.query(db, query)
    num_columns = length(results_imp)
    if length(frame.columns) == 0
      correct = (length(results_imp[1]) == 0)
    else
      results_sqlite = Relation(tuple((convert(typeof(results_imp.columns[ix]), frame[ix].values) for ix in 1:num_columns)...), num_columns)
      (imp_only, sqlite_only) = Data.diff(results_imp, results_sqlite)
      imp_only = map((c) -> c[1:min(10, length(c))], imp_only)
      sqlite_only = map((c) -> c[1:min(10, length(c))], sqlite_only)
      correct = (imp_only == sqlite_only)
    end
    println("$query_name $correct")
  end
end
```

```
1a true
1b false
1c true
1d false
2a true
2b true
2c true
2d true
3a false
3b false
3c false
4a true
4b true
4c true
5a true
5b true
5c false
6a true
6b true
6c true
6d true
6e true
6f false
7a true
7b true
7c false
8a false
8b true
8c false
8d true
9a false
9b false
9c false
9d true
10a true
10b true
10c false
11a false
11b false
11c true
11d false
12a true
12b true
12c true
13a true
13b false
13c true
13d true
14a true
14b true
14c true
15a true
15b true
15c false
15d false
16a true
16b false
16c true
16d true
17a false
17b false
17c true
17d false
17e true
17f false
18a false
18b false
19a false
19b true
19c false
19d false
20a false
20b true
20c true
21a false
21b false
21c false
22a false
22b false
22c false
22d false
23a true
23b true
23c true
24a false
24b false
25a true
25b true
25c true
26a true
26b true
26c true
27a false
27b false
27c false
28a false
28b true
28c false
29a true
29b true
29c false
30a false
30b true
30c true
31a true
31b true
31c true
32a true
32b true
33a true
33b true
33c true
```

SQLite isn't even playing the same game here. I'm going to have to exclude it from the results.

If we just compare Imp to Postgres, and look at relative times:

```
imp	pg (fk only)	pg (all)
1.00	2.20	2.27
1.00	11.37	10.11
1.00	12.02	12.55
1.00	9.71	8.83
1.00	10.11	9.52
1.00	9.91	9.33
1.00	10.38	9.88
1.00	10.93	10.31
1.00	1.94	1.91
1.00	2.44	2.39
1.00	2.51	2.46
1.00	2.53	2.46
3.12	4.31	1.00
1.00	2.57	2.50
1.00	2.05	2.06
1.00	2.06	2.08
1.00	2.49	2.33
1.00	9.68	2.72
4.73	1.15	1.00
1.00	298.62	13.07
1.00	6.42	6.28
1.00	9.81	3.82
1.00	2.34	2.30
1.00	329.97	2.20
1.00	123.88	1.00
1.00	6.09	6.10
1.00	80.14	79.75
1.00	11.00	5.37
1.00	2.47	2.45
1.00	12.55	12.28
3.38	2.16	1.00
1.88	2.20	1.00
1.00	1.80	1.80
1.00	3.36	3.32
1.00	1.63	1.56
1.00	2.16	1.87
2.17	1.02	1.00
1.00	7.97	5.35
1.00	8.94	6.10
1.00	5.82	4.87
1.00	5.00	4.49
1.00	1.89	1.49
1.00	1.89	1.93
1.00	5.15	4.96
1.00	4.55	4.42
1.00	1.05	1.04
1.00	1.03	1.02
1.00	1.55	1.50
1.00	4.41	4.09
1.00	6.53	5.80
1.00	3.63	3.35
1.00	1.29	1.29
1.00	11.03	1.20
5.78	1.00	1.00
4.64	1.00	1.00
1.00	17.72	15.97
1.00	2.39	2.34
1.00	2.93	2.84
1.00	3.00	2.91
1.00	7.24	7.13
1.00	5.11	5.07
1.00	4.99	4.91
1.00	4.94	4.87
1.00	2.54	2.47
1.00	6.18	6.09
1.00	12.42	12.47
1.00	3.88	2.96
2.11	2.19	1.00
2.34	1.69	1.00
1.00	2.90	2.54
1.00	15.28	15.03
1.00	4.57	4.47
1.00	8.48	8.21
1.00	5.69	5.44
1.00	6.26	3.83
1.00	5.32	3.78
1.00	5.26	3.75
1.00	1.68	1.52
1.00	3.79	3.50
1.00	2.11	1.85
2.05	1.07	1.00
1.00	1.43	1.44
1.00	2.44	1.55
1.55	1.01	1.00
1.40	1.25	1.00
1.00	23.49	2.26
1.00	3.49	3.44
1.00	19.63	12.16
1.00	1.32	1.33
1.00	11.86	19.53
1.00	13.55	2.46
1.00	9.34	7.60
1.00	2.97	5.26
1.00	5.03	12.62
1.00	3.08	9.53
1.00	15.70	3.66
1.00	2.60	6.77
1.00	6.00	2.11
1.00	557.94	2135.18
1.00	76.70	92.30
1.00	33.80	33.03
1.00	7.64	6.99
1.00	9.46	6.05
1.22	1.00	1.00
1.00	6.58	6.24
1.00	5.51	5.13
1.00	4.09	3.92
1.00	2437.95	22.39
1.00	7.66	6.96
2.40	1.03	1.00
3.14	2.19	1.00
24.41	1.00	1.57
```

I'd say 8a, 29a and 29b are clear failures for postgres. 33c is a clear failure for Imp. Otherwise, I think it's fair to say that both imp and pg have good plans, with Imp leading by a constant factor that's most likely due to the in-memory, read-optimized indexes rather than the query compiler itself.

Let's see what pg did different for 33c.

```
Aggregate  (cost=3988.02..3988.03 rows=1 width=84)
  ->  Nested Loop  (cost=927.04..3988.01 rows=1 width=84)
        ->  Nested Loop  (cost=926.62..3987.55 rows=1 width=69)
              ->  Nested Loop  (cost=926.19..3986.89 rows=1 width=77)
                    Join Filter: (it2.id = mi_idx2.info_type_id)
                    ->  Nested Loop  (cost=925.76..3979.64 rows=13 width=71)
                          ->  Nested Loop  (cost=925.34..3970.07 rows=21 width=56)
                                Join Filter: (kt1.id = t1.kind_id)
                                ->  Nested Loop  (cost=925.34..3966.76 rows=74 width=60)
                                      Join Filter: (ml.movie_id = t1.id)
                                      ->  Nested Loop  (cost=924.91..3927.85 rows=74 width=51)
                                            ->  Index Scan using index_info_type_id on info_type it2  (cost=0.14..14.12 rows=1 width=4)
                                                  Filter: ((info)::text = 'rating'::text)
                                            ->  Nested Loop  (cost=924.77..3912.99 rows=74 width=47)
                                                  Join Filter: (ml.movie_id = mc1.movie_id)
                                                  ->  Hash Join  (cost=924.34..3903.16 rows=15 width=39)
                                                        Hash Cond: (t2.kind_id = kt2.id)
                                                        ->  Nested Loop  (cost=923.22..3901.70 rows=54 width=43)
                                                              ->  Hash Join  (cost=922.79..3354.28 rows=130 width=18)
                                                                    Hash Cond: (mi_idx1.info_type_id = it1.id)
                                                                    ->  Merge Join  (cost=920.37..3295.38 rows=14713 width=22)
                                                                          Merge Cond: (mi_idx1.movie_id = ml.movie_id)
                                                                          ->  Index Scan using index_movie_info_idx_movie_id on movie_info_idx mi_idx1  (cost=0.43..43808.01 rows=1380035 width=14)
                                                                          ->  Sort  (cost=919.92..932.42 rows=5000 width=8)
                                                                                Sort Key: ml.movie_id
                                                                                ->  Nested Loop  (cost=37.49..612.73 rows=5000 width=8)
                                                                                      ->  Seq Scan on link_type lt  (cost=0.00..1.25 rows=3 width=4)
                                                                                            Filter: ((link)::text = ANY ('{sequel,follows,"followed by"}'::text[]))
                                                                                      ->  Bitmap Heap Scan on movie_link ml  (cost=37.49..185.08 rows=1875 width=12)
                                                                                            Recheck Cond: (link_type_id = lt.id)
                                                                                            ->  Bitmap Index Scan on index_movie_link_link_type_id  (cost=0.00..37.02 rows=1875 width=0)
                                                                                                  Index Cond: (link_type_id = lt.id)
                                                                    ->  Hash  (cost=2.41..2.41 rows=1 width=4)
                                                                          ->  Seq Scan on info_type it1  (cost=0.00..2.41 rows=1 width=4)
                                                                                Filter: ((info)::text = 'rating'::text)
                                                              ->  Index Scan using index_title_id on title t2  (cost=0.43..4.20 rows=1 width=25)
                                                                    Index Cond: (id = ml.linked_movie_id)
                                                                    Filter: ((production_year >= 2000) AND (production_year <= 2010))
                                                        ->  Hash  (cost=1.09..1.09 rows=2 width=4)
                                                              ->  Seq Scan on kind_type kt2  (cost=0.00..1.09 rows=2 width=4)
                                                                    Filter: ((kind)::text = ANY ('{"tv series",episode}'::text[]))
                                                  ->  Index Scan using index_movie_companies_movie_id on movie_companies mc1  (cost=0.43..0.59 rows=5 width=8)
                                                        Index Cond: (movie_id = mi_idx1.movie_id)
                                      ->  Index Scan using index_title_id on title t1  (cost=0.43..0.51 rows=1 width=25)
                                            Index Cond: (id = mc1.movie_id)
                                ->  Materialize  (cost=0.00..1.10 rows=2 width=4)
                                      ->  Seq Scan on kind_type kt1  (cost=0.00..1.09 rows=2 width=4)
                                            Filter: ((kind)::text = ANY ('{"tv series",episode}'::text[]))
                          ->  Index Scan using index_company_name_id on company_name cn1  (cost=0.42..0.45 rows=1 width=23)
                                Index Cond: (id = mc1.company_id)
                                Filter: ((country_code)::text <> '[us]'::text)
                    ->  Index Scan using index_movie_info_idx_movie_id on movie_info_idx mi_idx2  (cost=0.43..0.53 rows=2 width=14)
                          Index Cond: (movie_id = t2.id)
                          Filter: (info < '3.5'::text)
              ->  Index Scan using index_movie_companies_movie_id on movie_companies mc2  (cost=0.43..0.62 rows=5 width=8)
                    Index Cond: (movie_id = t2.id)
        ->  Index Scan using index_company_name_id on company_name cn2  (cost=0.42..0.44 rows=1 width=23)
              Index Cond: (id = mc2.company_id)
```

Not all that different. It puts the production year and country code higher up than I did, but copying that doesn't help much. 

I thought maybe the problem here is that I hitting all the movie_companies and all the infos before I get to return, so there are going to be huge numbers of dupes. But no, I'm only producing 114 rows total for 96 unique results.

Oh, I guess I'm also redoing all the t1 work for every t2 company. That seems like a bad idea. Let's factor it out into multiple queries:

``` julia
function q33c_factored()
  rating_type = @query begin 
    info_type.info(it, "rating")
    return (it::Int8,)
  end
  kind_types = @query begin
    kt_kind in ["tv series", "episode"]
    kind_type.kind(kt, kt_kind)
    return (kt::Int8,)
  end
  movie_links = @query begin
    link in ["sequel", "follows", "followed by"]
    link_type.link(lt, link)
    movie_link.link_type(ml, lt)
    return (ml::Int16,)
  end
  linked_movies = @query begin
    rating_type(it)
    movie_links(ml)
    movie_link.linked_movie(ml, t2)
    title.kind(t2, kt)
    kind_types(kt)
    title.production_year(t2, production_year)
    @when 2000 <= production_year <= 2010
    movie_info_idx.movie(mii2, t2)
    movie_info_idx.info_type(mii2, it)
    movie_info_idx.info(mii2, rating2)
    @when rating2 < "3.5"
    return (ml::Int16, t2::Int32, rating2::String)
  end
  linking_movies = @query begin
    rating_type(it)
    linked_movies(ml, _, _)
    movie_link.movie(ml, t1)
    title.kind(t1, kt)
    kind_types(kt)
    movie_companies.movie(mc1, t1)
    movie_companies.company(mc1, cn1)
    company_name.country_code(cn1, code)
    @when code != "[us]"
    title.title(t1, title1)
    movie_info_idx.movie(mii1, t1)
    movie_info_idx.info_type(mii1, it)
    movie_info_idx.info(mii1, rating1)
    company_name.name(cn1, name1)
    return (ml::Int16, t1::Int32, name1::String, rating1::String, title1::String)
  end
  @query begin
    linking_movies(ml, t1, name1, rating1, title1)
    linked_movies(ml, t2, rating2)
    title.title(t2, title2)
    movie_companies.movie(mc2, t2)
    movie_companies.company(mc2, cn2)
    company_name.name(cn2, name2)
    return (name1::String, name2::String, rating1::String, rating2::String, title1::String, title2::String)
  end
end
```

```
@benchmark(q33c_factored()) = BenchmarkTools.Trial: 
  samples:          1957
  evals/sample:     1
  time tolerance:   5.00%
  memory tolerance: 1.00%
  memory estimate:  89.27 kb
  allocs estimate:  510
  minimum time:     1.69 ms (0.00% GC)
  median time:      2.44 ms (0.00% GC)
  mean time:        2.55 ms (2.67% GC)
  maximum time:     22.87 ms (88.64% GC)
```

That's about 20x faster than postgres and 320x faster than the original. Maybe I should go ahead and write a factorizing compiler?

[Tuning postgres](https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server) resulted in worse performance.

### 2016 Oct 6

I wrote up my results so far and showed them to a few people, and everyone I showed it to said that the report was too short and didn't make sense without going into the code. 

But the code is really ugly. So I further delaying the report by going back to clean things up.

I realized I could simplify a lot of the codegen if I was willing to give up on the depth-first model and eat some extra allocations in the query - one buffer per variable. I'm now running an entire intersection at once:

``` julia
immutable Intersection{C, B}
  columns::C
  buffer::B
end

function Intersection(columns)
  buffer = Vector{NTuple{length(columns), UnitRange{Int64}}}()
  Intersection(columns, buffer)
end

function project(column::Vector, range, val)
  lo = gallop(column, val, range.start, range.stop, 0)
  hi = gallop(column, val, lo, range.stop, 1)
  lo:hi
end

@generated function project_at{N}(intersection, ranges::NTuple{N}, val)
  :(@ntuple $N ix -> project(intersection.columns[ix], ranges[ix], val))
end

function intersect_at(intersection, ranges)
  empty!(intersection.buffer)
  min_ix = indmin(map(length, ranges))
  while ranges[min_ix].start < ranges[min_ix].stop
    val = intersection.columns[min_ix][ranges[min_ix].start]
    projected_ranges = project_at(intersection, ranges, val)
    if all(r -> r.start < r.stop, projected_ranges)
      push!(intersection.buffer, projected_ranges)
    end
    ranges = map((old, new) -> new.stop:old.stop, ranges, projected_ranges)
  end 
  intersection.buffer
end

function intersect_at(intersection, ranges, val)
  empty!(intersection.buffer)
  projected_ranges = project_at(intersection, ranges, val)
  if all(r -> r.start < r.stop, projected_ranges)
    push!(intersection.buffer, projected_ranges)
  end
  intersection.buffer
end

function val_at(intersection, ranges)
  intersection.columns[1][ranges[1].start]
end
```

This replaces both the finger interface and much of the codegen. The generated code for the queries is now a little more readable:

``` julia
begin  # /home/jamie/imp/src/Query.jl, line 339:
    let  # /home/jamie/imp/src/Query.jl, line 340:
        local #2173#index_1 = (Query.index)(edge,[1,2])
        local #2179#range_1_0 = 1:(Query.length)(#2173#index_1[1]) + 1
        local #2174#index_3 = (Query.index)(edge,[1,2])
        local #2175#index_5 = (Query.index)(edge,[2,1])
        local #2180#range_3_0 = 1:(Query.length)(#2174#index_3[1]) + 1
        local #2181#range_5_0 = 1:(Query.length)(#2175#index_5[1]) + 1 # /home/jamie/imp/src/Query.jl, line 341:
        local #2182#intersection_a = (Query.Intersection)((Query.tuple)(#2173#index_1[1],#2175#index_5[2]))
        local #2183#intersection_b = (Query.Intersection)((Query.tuple)(#2173#index_1[2],#2174#index_3[1]))
        local #2184#intersection_c = (Query.Intersection)((Query.tuple)(#2174#index_3[2],#2175#index_5[1])) # /home/jamie/imp/src/Query.jl, line 342:
        local #2176#results_1 = (Query.Vector){Int64}()
        local #2177#results_2 = (Query.Vector){Int64}()
        local #2178#results_3 = (Query.Vector){Int64}() # /home/jamie/imp/src/Query.jl, line 343:
        begin  # /home/jamie/imp/src/Query.jl, line 327:
            for (#2185#range_1_1,#2186#range_5_1) = (Query.intersect_at)(#2182#intersection_a,(#2179#range_1_0,#2181#range_5_0)) # /home/jamie/imp/src/Query.jl, line 328:
                local a = (Query.val_at)(#2182#intersection_a,(#2185#range_1_1,#2186#range_5_1)) # /home/jamie/imp/src/Query.jl, line 329:
                begin  # /home/jamie/imp/src/Query.jl, line 327:
                    for (#2187#range_1_2,#2188#range_3_1) = (Query.intersect_at)(#2183#intersection_b,(#2185#range_1_1,#2180#range_3_0)) # /home/jamie/imp/src/Query.jl, line 328:
                        local b = (Query.val_at)(#2183#intersection_b,(#2187#range_1_2,#2188#range_3_1)) # /home/jamie/imp/src/Query.jl, line 329:
                        if a < b # /home/jamie/imp/src/Query.jl, line 292:
                            begin  # /home/jamie/imp/src/Query.jl, line 327:
                                for (#2189#range_3_2,#2190#range_5_2) = (Query.intersect_at)(#2184#intersection_c,(#2188#range_3_1,#2186#range_5_1)) # /home/jamie/imp/src/Query.jl, line 328:
                                    local c = (Query.val_at)(#2184#intersection_c,(#2189#range_3_2,#2190#range_5_2)) # /home/jamie/imp/src/Query.jl, line 329:
                                    begin  # /home/jamie/imp/src/Query.jl, line 298:
                                        #2191#need_more_results = true # /home/jamie/imp/src/Query.jl, line 299:
                                        if b < c # /home/jamie/imp/src/Query.jl, line 292:
                                            begin  # /home/jamie/imp/src/Query.jl, line 281:
                                                (Query.push!)(#2176#results_1,a)
                                                (Query.push!)(#2177#results_2,b)
                                                (Query.push!)(#2178#results_3,c) # /home/jamie/imp/src/Query.jl, line 283:
                                                #2191#need_more_results = false
                                            end
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end # /home/jamie/imp/src/Query.jl, line 344:
        (Query.Relation)((Query.tuple)(#2176#results_1,#2177#results_2,#2178#results_3),3)
    end
end
end
```

As a bonus, compilation time is waaaaay lower.

As usual, there are type inference failures and I'll pick them off one by one.

I can't get proper benchmarks because I'm on a bus and benchmarking without mains power is sketchy, but it looks like this might actually be faster too. I had no idea in advance which way it would fall. There is more cache pressure from filling the buffers, but there is less code and better code locality (because it runs an entire intersection at once rather than jumping back and forth between different intersections) and I also reduced the number of comparisons in project (which could also have been done for the old version).

I'm also down to about 600 lines of code, of which maybe 200 can be deleted once unboxing improves.

Argh. I was seeing really big slowdowns on some of the queries and started to panic a little, but it turns out I forgot to include the `return_after` optimization when I changed to bfs. This uglifies the code again. It would be prettier to wrap the return_after part in a function and call return to break out of the loop, but then I would lose the ability to easily check the code with `@code_warntype`. 

``` julia 
if typeof(clause) == Assign
  body = (quote let
    local $(esc(var)) = $(esc(clause.expr))
    for $after_ranges in intersect_at($intersection, $before_ranges, $(esc(var)))
      $body
    end
  end end).args[2]
elseif typeof(clause) == In
  body = (quote let
    local iter = $(esc(clause.expr))
    local state = start(iter)
    while $need_more_results && !done(iter, state)
      ($(esc(var)), state) = next(iter, state)
      for $after_ranges in intersect_at($intersection, $before_ranges, $(esc(var)))
        $body
      end
    end
  end end).args[2]
else
  body = (quote let
    local iter = intersect_at($intersection, $before_ranges)
    local state = start(iter)
    local $(after_ranges.args...)
    while $need_more_results && !done(iter, state)
      ($after_ranges, state) = next(iter, state)
      local $(esc(var)) = val_at($intersection, $after_ranges)
      $body
    end
  end end).args[2]
end
```

Bah, still slow. Back to dfs :(

I noticed a problem with my benchmarks. On longer queries, it seems to always run exactly two samples and then include a gc in the second sample, regardless of how much allocation the function performed. So those queries are being charged half of the cost of a full gc, instead of it being amortized over the number of executions it would take to trigger a gc. This *might* be why the bfs version with buffers looked like it had such large regressions.

Notably, I reran 33c and got 6.5ms instead of 740ms.

### 2016 Oct 10

Clear signs of wheel-spinning last week. Most of the last month has been spent trying to persuade Julia to generate the code that I want. That was waste of effort. I'm just going to generate it myself. The resulting code is ugly but I'll live. The report can just explain the problem, and I can write another post when the unboxing pull-request lands in Julia. It also means that there is no longer a clear interface between indexes and queries, but I only wanted that to experiment with different indexes anyway and I'm not doing that anymore. I also switched back to triejoin so I don't have to worry about correctly counting keys.

New benchmarks:

```
"1a" 1.41 ms
"1b" 16.31 μs
"1c" 318.84 μs
"1d" 17.51 μs
"2a" 49.72 ms
"2b" 47.00 ms
"2c" 37.53 ms
"2d" 71.73 ms
"3a" 98.62 ms
"3b" 56.26 ms
"3c" 140.12 ms
"4a" 58.80 ms
"4b" 57.01 ms
"4c" 61.19 ms
"5a" 18.07 ms
"5b" 18.06 ms
"5c" 37.87 ms
"6a" 2.44 ms
"6b" 1.12 s
"6c" 24.37 μs
"6d" 1.19 s
"6e" 2.42 ms
"6f" 3.39 s
"7a" 3.20 ms
"7b" 2.28 ms
"7c" 568.99 ms
"8a" 21.99 ms
"8b" 22.16 ms
"8c" 3.46 s
"8d" 365.90 ms
"9a" 248.63 ms
"9b" 85.12 ms
"9c" 267.16 ms
"9d" 842.88 ms
"10a" 389.45 ms
"10b" 110.14 ms
"10c" 19.76 s
"11a" 7.34 ms
"11b" 6.40 ms
"11c" 32.91 ms
"11d" 33.13 ms
"12a" 187.78 ms
"12b" 80.83 μs
"12c" 264.14 ms
"13a" 697.97 ms
"13b" 1.15 s
"13c" 1.15 s
"13d" 3.25 s
"14a" 82.53 ms
"14b" 25.27 ms
"14c" 178.19 ms
"15a" 650.16 ms
"15b" 1.65 ms
"15c" 3.69 s
"15d" 2.99 s
"16a" 21.67 ms
"16b" 8.35 s 
"16c" 566.33 ms
"16d" 449.61 ms
"17a" 2.05 s
"17b" 2.10 s
"17c" 2.02 s
"17d" 1.99 s
"17e" 4.81 s
"17f" 2.27 s
"18a" 442.67 ms
"18b" 109.62 ms
"19a" 406.24 ms
"19b" 292.64 ms
"19c" 408.73 ms
"19d" 806.69 ms
"20a" 969.01 ms
"20b" 346.08 ms
"20c" 300.93 ms
"21a" 13.42 ms
"21b" 12.49 ms
"21c" 13.81 ms
"22a" 172.89 ms
"22b" 97.67 ms
"22c" 470.89 ms
"22d" 739.10 ms
"23a" 109.80 ms
"23b" 8.32 ms
"23c" 253.05 ms
"24a" 217.09 ms
"24b" 819.59 μs
"25a" 1.10 s
"25b" 40.71 ms
"25c" 8.20 s
"26a" 125.42 ms
"26b" 24.10 ms
"26c" 310.49 ms
"27a" 9.27 ms
"27b" 7.44 ms
"27c" 9.32 ms
"28a" 230.57 ms
"28b" 120.77 ms
"28c" 156.38 ms
"29a" 66.33 μs
"29b" 65.88 μs
"29c" 15.00 ms
"30a" 786.33 ms
"30b" 110.01 ms
"30c" 4.59 s
"31a" 405.43 ms
"31b" 130.09 ms
"31c" 594.81 ms
"32a" 4.63 μs
"32b" 20.02 ms
"33a" 2.20 ms
"33b" 56.88 ms
"33c" 5.10 ms
```

Following the advice of [malisper](https://github.com/malisper) I disabled the genetic optimizer in postgres, increasing the query planning time to multiple seconds but dramatically improving the quality of the plans. I also increased the size of `shared_buffers` to 8gb and added an assertion in the benchmark harness that there are no buffer misses. 

``` julia
function bench_pg(qs = query_names())
  medians = []
  for query_name in qs
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    bench = "explain (analyze, buffers) $query"
    cmd = `sudo -u postgres psql -c $bench`
    times = Float64[]
    @show query_name now()
    readstring(cmd)
    trial = @benchmark begin
      result = readstring($cmd)
      time = parse(Float64, match(r"Execution time: (\S*) ms", result)[1])
      missed_buffer = ismatch(r"Buffers [^\n]* read=", result)
      if missed_buffer == true
        println("Miss!")
        println(result)
        @assert false
      end
      push!($times, time) 
    end evals=3
    @show trial
    push!(medians, median(times))
  end
  medians
end
```

Updated results for pg:

```
Any[8.384,0.488,6.06,0.4995,579.993,574.173,562.608,717.823,148.766,108.611,262.496,121.608,17.775,126.95,205.613,204.72,233.988,17.2675,179.119,1.443,4020.29,17.279,4420.67,9.888,3.5715,3290.45,2001.78,161.021,9185.58,5408.18,303.381,294.729,1779.26,5369.51,388.797,175.678,7214.28,56.2895,34.1675,186.62,178.331,275.39,1.138,1324.61,3560.86,1227.57,1125.06,5442.63,261.023,112.609,593.089,679.625,8.149,646.972,843.843,174.935,21178.3,1719.74,1381.67,11364.6,4670.83,4392.36,4418.51,10329.0,8133.33,7233.86,235.051,301.255,184.303,1619.21,16241.8,2174.88,1526.24,890.889,72.671,57.3285,63.346,411.781,233.087,1987.7,925.014,391.474,43.981,378.682,541.283,483.223,2302.06,286.858,6186.31,1573.1,119.445,1321.33,56.237,60.613,33.318,1079.37,719.925,879.48,56.557,5.4465,1775.88,2642.17,363.429,2466.51,2592.47,365.791,2624.17,0.165,143.5,30.939,33.864,35.1965]
```

Some queries are better, some are worse.

I finished up the report. Just waiting for a last round of feedback and some non-1am editing for flow.

### 2016 Oct 24

Ok, let's get going again.

Last month I used the benchmark against postgres as a goal to work towards. I wrote the blog post first and then spent the month working to make everything in it true. 

Today I started doing the same for the project as whole. I put up a [sketch](https://github.com/jamii/jamii.github.com/blob/master/_drafts/imp.markdown) of what I want the eventual overview to look like. 

I also need to pick another short-term goal. I'm leaning towards building a simple stock exchange and trading interface. It's the kind of thing that someone might want to do in a spreadsheet, but it's made difficult by the variable size collections and quantity of data. 

Here's a sketch of the core logic for a single market:

``` julia
bitstype 64 Order
@enum Side Buy Sell

@relation order(Order) => (DateTime, Dec64, Int64, Side)
@relation matched(Order, Order) => (Dec64, Int64)
@relation remaining(Side, Dec64, DateTime, Order) => Int64

@query begin
  @minimum remaining(Buy, buy_price, buy_time, buy_order) => buy_quantity
  @maximum remaining(Sell, sell_price, sell_time, sell_order) => sell_quantity
  @when buy_price >= sell_price
  price = (buy_time < sell_time) ? buy_price : sell_price
  quantity = min(buy_quantity, sell_quantity)
  return matched(buy_order, sell_order) => (price, quantity)
end

@query begin
  order(order) => (time, price, quantity, side)
  bought = @query match(order, matched_order) => (_, matched_quantity)
  sold = @query match(matched_order, order) => (_, matched_quantity)
  remaining = quantity - sum(bought[3]) - sum(sold[3])
  @when remaining > 0
  return remaining(side, price, time, order) => remaining
end
```

`@minimum` doesn't currently exist and is awkward to fake using `=` and `@query`, so now is a good time to figure out what to do with aggregates in general.

I want to think of aggregates in general as functions that take a query and return a new relation (possibly containing only one row in the case of simple aggregates like sum), but ideally without having to allocate the intermediate relation. 

Meanwhile, I belatedly discovered that Julia has an undocumented goto macro, which means I don't have to manually desugar loops in order to break out in a controlled way. 

### 2016 Oct 25

Here are some things that I might want to do with a query:

* Materialize the results into a relation
* Aggregate the results into a single value
* Take the first result, last results, 42nd-57th results etc
* Check whether a specific value is in the results 
* Materialize the factorized results, without computing the full results 

At the moment I'm struggling because I'm mashing them all into a single chunk of codegen. The original triejoin paper instead defines an trie iterator protocol that both indexes and queries implement. If I did something similar, I could just generate code for the iterator and implement the materialization, aggregates, first/last/range etc as functions of the query. 

That feels like another huge compiler time-sink though. Not sure that I want to dive into that again.

### 2016 Nov 7

Wow, a month without any real work. For the sake of just getting something done, I started on a little debugging UI:

``` julia
function debug(relation)
  @relation displaying() => (Int64, String)
  
  @query return displaying() => (0, relation[1][1])
  
  @Window(displaying) do window, event_number
    
    header = @query begin
      relation(name) => _
      displayed = @exists displaying() => (_, $name)
      style = displayed ? "font-weight: bold" : ""
      node = button(Dict(:onclick => @event(displaying() => name), :style => style), name)
      return (name::String,) => node::Node
    end
    
    grid = @query begin
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      style = "margin-left: 2em"
      node = vbox(map((v) -> Hiccup.div(Dict(:style=>style), string(v)), relation[c]))
      return (c::Int64,) => node::Node
    end
    
    Blink.body!(window, vbox([hbox(header[2]), hbox(grid[2])]))
    
  end
end
```

It required tweaking a couple of things elsewhere. Most notably, I changed event push to take account of unique keys properly, so that I only ever store the last chosen relation here.

### 2016 Nov 8

Got most of the way towards making the tables editable, but got bogged down in the details of passing data back and forth between Julia and the browser. I didn't really think through events properly.

### 2016 Nov 9 

Really crude awful committing now works for the editor. I never got around to writing a delete function for relations so it only does the right thing if the unique key is left unchanged.

I tried to switch to Escher for the dom diffing but after a few hours ended up with just a [handful of filed issues](https://github.com/shashi/Escher.jl/issues/created_by/jamii) and no hello world. 

Trying to update some of the other packages I rely on and finding that package manager is hanging when trying to clone. Cloning the same url directly with git at the command line works fine. No idea what's going on there.

Frustrating.

### 2016 Nov 22

Eugh. Is this thing still on?

The way I've been working with relations at the moment requires blowing away all the state and opening a new UI window whenever I change anything. Clearly that's not good.

To fix this, I have to switch from the current imperative updates to something more declarative. The input state is all going to want to go in a data-structure somewhere, which means that the queries need to be amended to read names out of that structure. I added a new macro that takes a query expression and returns a callable object with some useful metadata.

``` julia
type View
  relation_names::Vector{Symbol}
  query
  code
  eval::Function
end

macro view(query)
  (code, relation_names) = plan_query(query)
  escs = [:($(esc(relation_name)) = $relation_name) for relation_name in relation_names]
  code = quote
    $(escs...)
    $code
  end
  :(View($relation_names, $(Expr(:quote, query)), $(Expr(:quote, code)), $(Expr(:->, relation_names..., code))))
end

function (view::View){R <: Relation}(state::Dict{Symbol, R})
  args = map((s) -> state[s], view.relation_names)
  view.eval(args...)
end
```

Then a bunch of these get wrapped together in a Flow:

``` julia
type Flow
  relations::Dict{Symbol, Relation}
  views::Vector{Pair{Symbol, View}} # TODO make views a dict, do topo sort
  cached::Dict{Symbol, Relation}
  watchers::Set{Any}
end

function Flow()
  Flow(Dict{Symbol, Relation}(), Vector{Pair{Symbol, View}}(), Dict{Symbol, Relation}(), Set{Any}())
end

function refresh(flow::Flow)
  old_cached = flow.cached
  cached = copy(flow.relations)
  for (name, view) in flow.views
    cached[name] = view(cached)
  end
  flow.cached = cached
  for watcher in flow.watchers
    watcher(old_cached, cached)
  end
end

function Base.getindex(flow::Flow, name::Symbol)
  flow.cached[name]
end

function Base.setindex!(flow::Flow, relation::Relation, name::Symbol)
  flow.relations[name] = relation
  refresh(flow)
end

function setviews(flow::Flow, views::Vector{Pair{Symbol, View}})
  flow.views = views
  refresh(flow)
end

function watch(watcher, flow::Flow)
  push!(flow.watchers, watcher)
end
```

The idea is to structure programs like this:

``` julia
flow = Flow()

# set up state
flow[:foo] = @relation ...
flow[:bar] = @relation ...

# set up views 
setviews(flow, [
  :quux => @view ...
])

# open a UI
window = Window(flow)
watch(flow) do _, cached
  Blink.body!(window, cached[:body][1][1])
end
```

Then re-evalling any part of the file results in updates to the currently open window, without blowing any state away. 

Right now this can't handle fixpoints or unions, because I've been doing those imperatively before, and it doesn't handle subqueries/aggregates because their metadata doesn't get parsed up in the view. Those are fixable. 

It also doesn't handle loops over time. The way that Dedalus and Eve handle time is by having a single, serial timeline built in to the language. (Eve has plans for distributed execution, but I don't think they've published anything yet). Time is used to break non-monotonic fixpoints, but also for efficient mutation and for functions that are more easily expressed in sequential form. Tying the latter to the single timeline seems problematic.

I much prefer the model in Timely Dataflow, which allows multiple loops and nested loops. I think this can be done in a way that allows writing reactive programs without baking time into the language.

### 2016 Nov 23

Quickly added subqueries to the syntax. Instead of writing:

``` julia
@view begin
  playlist(p, pn)
  tracks = @query begin 
    playlist_track($p, t)
    track(t, _, _, _, _, _, _, _, price)
    return (t::Int64, price::Float64)
  end
  total = sum(tracks[2])
  return (pn::String, total::Float64)
end
```

I can now write:

``` julia
@view begin
  playlist(p, pn)
  @query begin 
    playlist_track(p, t)
    track(t, _, _, _, _, _, _, _, price)
    return (t::Int64, price::Float64)
  end
  total = sum(price)
  return (pn::String, total::Float64)
end
```

And the subquery metadata gets propagated up to the final view.

At least in theory. In practice I broke something somewhere and it's pulling NaNs out of thin air...

### 2016 Nov 24

It was variable clashes between the two macros. I suspected as much, but actually figuring out which variable was beyond me last night. Fixed now.

The compiler is an unholy mess. The root of the problem is that without stack allocation it's expensive to abstract out any part of the query into functions. Instead I have to mash everything together into one huge function and carefully keep track of scopes and variable collisions myself. I don't know what to do about that, short of just accepting the performance hit. Based on the slowdowns I had whenever I allocated by mistake, it might about an order of magnitude.

I tweaked the way flows work so that views always merge into some existing relation rather than defining a new one, which gives us back union, but then reconsidered after thinking about the effects on debugging - it means that the value of a given view changes during flow refresh.

It's easy to write union in the current setup:

``` julia
type Union <: AbstractView
  views::Vector{AbstractView}
end

function (union::Union)(inputs::Dict{Symbol, Relation})
  reduce(merge, (view(inputs) for view in union.views))
end
```

Writing fixpoint is less easy, because it doesn't know what output name it's going to be assigned. It's also impossible to write fixpoint over multiple views because the current interface only allow returning one result. Ooops.

I've made and undone changes for an hour or two now. I think it's clear I need to sit down and think about exactly what I want out of this.

### 2016 Dec 11

So I've made some janky flow stuff that I don't really like but will live with for the sake of momentum.

``` julia
abstract Flow

type Create <: Flow
  output_name::Symbol
  input_names::Vector{Symbol}
  meta::Any
  eval::Function
end

type Merge <: Flow
  output_name::Symbol
  input_names::Vector{Symbol}
  meta::Any
  eval::Function
end

type Sequence <: Flow
  flows::Vector{Flow}
end

type Fixpoint <: Flow
  flow::Flow
end

function output_names(create::Create)
  Set(create.output_name)
end

function output_names(merge::Merge)
  Set(merge.output_name)
end

function output_names(sequence::Sequence)
  union(map(output_names, sequence.flows)...)
end

function output_names(fixpoint::Fixpoint)
  output_names(fixpoint.flow)
end

function (create::Create)(inputs::Dict{Symbol, Relation})
  output = create.eval(map((name) -> state[name], create.input_names))
  inputs[create.output_name] = output
end

function (merge::Merge)(inputs::Dict{Symbol, Relation})
  output = merge.eval(map((name) -> state[name], merge.input_names))
  inputs[merge.output_name] = merge(inputs[merge.output_name], output)
end

function (sequence::Sequence)(inputs::Dict{Symbol, Relation})
  for flow in sequence.flows
    flow(inputs)
  end
end

function (fixpoint::Fixpoint)(inputs::Dict{Symbol, Relation})
  names = output_names(fixpoint.flow)
  while true
    old_values = map((name) -> inputs[name], names)
    fixpoint.flow(inputs)
    new_values = map((name) -> inputs[name], names)
    if old_values == new_values
      return
    end
  end
end

function query_to_flow(constructor, query)
  (clauses, vars, created_vars, input_names, return_clause) = Query.parse_query(query)
  code = Query.plan_query(clauses, vars, created_vars, input_names, return_clause, Set())
  escs = [:($(esc(input_name)) = $input_name) for input_name in input_names]
  code = quote
    $(escs...)
    $code
  end
  :($constructor(return_clause.name, $(collect(input_names)), $(Expr(:quote, query)), $(Expr(:->, Expr(:tuple, input_names...), code))))
end

macro create(query)
  query_to_flow(Create, query)
end

macro merge(query)
  query_to_flow(Merge, query)
end
```

I realised that with the way I have things setup currently, I can't write code like I did before that reacts to events and mutates the world state. I think this is probably a good thing. Let's see how far I can get with all mutation confined to the immediate effects of user interaction. A little like the Monolog experiment I worked on earlier this year.

I got it all hooked up to UI too, so I can write things like this:

``` julia
world = World()

world[:window] = Relation(([span("hello")],), 1)

window(world)

world[:window] = Relation(([span("world")],), 1)
```

Next up is redoing the table interface in this new style.

### 2016 Dec 12

Here it is:

``` julia
world = World()

world[:displaying] = @relation () => String

world[:cell] = @relation (Int64, Int64) => Hiccup.Node
world[:row] = @relation (Int64,) => Hiccup.Node
world[:tab] = @relation (String,) => Hiccup.Node
world[:window] = @relation () => Hiccup.Node

begin 
  setflow(world, Sequence([
    @create begin 
      name in map(string, keys(world.outputs))
      node = button(Dict(:onclick=>@event displaying() => name), name)
      return tab(name) => node
    end
  
    @create begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      r in 1:length(column)
      value = column[r]
      style = "height: 2em; flex: $(100/length(columns))%"
      cell = Hiccup.div(Dict(:style=>style), render_value(value))
      return cell(c, r) => cell
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      typ = eltype(column)
      style = "border-bottom: 1px solid #aaa; height: 2em; flex: $(100/length(columns))%"
      node = Hiccup.div(Dict(:style=>style), string(typ))
      return cell(c, 0) => node
    end
    
    @create begin
      cell(_, r) => _
      @query cell(c, r) => cell_node
      row = hbox(cell_node)
      return row(r) => row
    end
  
    @create begin
      @query tab(name) => tab_node
      tabs = hbox(tab_node)
      @query row(r) => row_node
      rows = vbox(row_node)
      window = vbox([tabs, rows])
      return window() => window
    end 
  ]))
end

window(world)
```

Live updating works nicely. 

It's missing row editing, but that didn't really work properly in the previous version anyway. Next thing I need to do is figure out how to handle how to deal with user input correctly.

### 2016 Dec 17

I have incremental dom patching sort-of working. The actual dom patching works, but somehow messages back to Julia are getting lost, and anything that waits on a message gets stuck eg `@js w console.log("ok")` hangs in a lock:

``` julia
InterruptException:
 in process_events(::Bool) at ./libuv.jl:82
 in wait() at ./event.jl:147
 in wait(::Condition) at ./event.jl:27
 in lock(::ReentrantLock) at ./lock.jl:74
 in (::Atom.##65#68)(::Dict{String,Any}) at /home/jamie/.julia/v0.5/Atom/src/eval.jl:107
 in handlemsg(::Dict{String,Any}, ::Dict{String,Any}, ::Vararg{Dict{String,Any},N}) at /home/jamie/.julia/v0.5/Atom/src/comm.jl:163
 in (::Atom.##14#17)() at ./event.jl:68
```

Annoyingly, the network tab in electron shell doesn't seem to be able to see the websocket. I can verify in wireshark that the message is sent to the server. The event handler doesn't get called. And... that's about it. What now?

Somehow I managed to figure out that the culprit was that `@js(window, morphdom(document.getElementById("main"), $html))` never returns. I don't know why. If I change it to an async call `@js_(window, morphdom(document.getElementById("main"), $html))` everything works fine. That sounds like a bug in Blink. I don't really want to dive into the networking code though, so I'm just gonna leave it and hope it doesn't happen again. That'll probably work...

### 2016 Dec 18

I found a problem with my approach to handling state. I wanted to have mutations occur only in event handlers, but the event handlers produce json and I need arbitrary Julia values. I could probably fix that by putting julia code strings in the event handler and evalling it on the server. Or I could just generate some unique id and store the events client-side. That seems more sensible. But then how do I get values out of eg textboxes?

The core problem here is the asynchrony between server and the client. The client knows what the current value of the textbox is. The server knows how to interpret the value. If other mutations have happened in the meantime, the server may have forgotten what the client is currently seeing, and might wrongly interpret the event (this is the problem with naive applications of CQRS where the events are things like "user 7 clicked on button 42"). 

I think it's clear that any model that allows mutation at all is still going to be subject to subtle asynchrony bugs, but I want to continue to allow it for now so that I don't have to figure out how to deal with time yet.

Maybe a better option would be to remove the asynchrony by using a native gui framework. It would be far easier to write this query if I could just access the value of the textbox in the query.

Maybe I should try to write a blog post on the problem to clear up my thinking.

### 2016 Dec 21

So I just gave up and introduced arbitrary state. I don't like it, but I don't see a way around it right now. Maybe I'll end up with something like Elm where the view and update functions are separated. 

I moved all the query code into the `@merge` macro, which works like before, and created `@state` and `@fresh` macros for creating stateful and stateless tables. (`@fresh` is a shitty name but `@view` is already taken in Julia. Maybe `@stateful` and `@transient`?)

With those I now have pretty decent editing working, but I'm having to splice in javascript to get the most recent state out of the textbox. 

``` julia 
@merge begin
  displaying() => name
  editing() => (name, c, r, value)
  columns = world[Symbol(name)].columns
  style = "height: 2em; flex: $(100/length(columns))%"
  onkeydown = """
    if (event.which == 13) {
      Blink.msg('event', {'table': 'editing', 'values': ['$name', $c, $r, this.value]}); 
      Blink.msg('event', {'table': 'committed', 'values': [true]}); 
      return false;
    }
    if (event.which == 27) {
      Blink.msg('event', {'table': 'editing', 'values': ['', 0, 0, '']});
      return false;
    } 
  """
  cell = textarea(Dict(:style=>style, :rows=>1, :onkeydown=>onkeydown), value)
  return cell(c, r) => cell
end
```

There's a similar problem in the other direction where if anything causes the flow to refresh, the textbox gets reset. 

Both problems are caused by the fact that I don't have synchronous access to the dom, so the model and the view can get out of sync. I could get synchronous access either by porting Imp to js or by using a native toolkit. Or I could figure out a way to deal with asynchronous access.

### 2016 Dec 26

The UI-as-a-value-embedded-in-the-debugger thing I was pushing for was interesting but a) it caused major problems with not having a vdom <-> dom bijection and b) when pressed I couldn't come up with any actual usecases that weren't just buttons. So let's scrap it and go with the flat relational representation that we used in Eve since way back in early 2014. 

Every DOM node gets a unique id:

``` julia
# typealias Id UInt

# macro id(args...)
#   h = :(zero(UInt))
#   for arg in args
#     h = :(hash($(esc(arg)), $h))
#   end
#   h
# end

# root = UInt(0)

typealias Id String

macro id(args...)
  :(join([$(map(esc,args)...)], "-"))
end

root = "root"
```

I haven't implemented multiple returns yet so I'll just mush everything important into one table:

``` julia
# (id) => (parent, ix, kind, class, text)
pre = @transient node(Id) => (Id, Int64, String, String, String)
```

The main program fills lots of stuff into that table, and then the UI lib sorts it by depth and sends it to the frontend:

``` julia
post = Sequence([
  # (level, parent, ix, id, kind, class, text)
  @transient sorted_node(Int64, Id, Int64, Id, String, String, String)

  @merge begin
    root = UI.root
    node(id) => (root, ix, kind, class, text)
    return sorted_node(1, root, ix, id, kind, class, text)
  end
  
  Fixpoint(
    @merge begin
      sorted_node(level, _, _, parent, _, _, _)
      node(id) => (parent, ix, kind, class, text)
      return sorted_node(level+1, parent, ix, id, kind, class, text)
    end
  )
])

function render(window, state)
  (_, id, parent, ix, kind, class, text) = state[:sorted_node].columns
  @js(window, render($id, $parent, $ix, $kind, $class, $text))
end
```

Then the frontend erases the old DOM and builds a new one from scratch:

``` js
function render(parent, ix, id, kind, className, textContent) {
    document.getElementById("root").innerHTML = "";
    for (var i = 0; i < parent.length; i++) {
        node = document.createElement(kind[i]);
        node.id = id[i];
        node.className = className[i];
        node.textContent = textContent[i];
        document.getElementById(parent[i]).appendChild(node);
    }
}
```

The next thing I have to do is make this incremental. It's not too hard - just diff the new sorted_nodes table against the old and then do deletions before insertions. 

I've ported the readonly parts of the table interface to this model:

``` julia
@merge begin
  root = UI.root
  return node(@id(:top)) => (root, 1, "div", "vbox", "")
end

@merge begin
  return node(@id(:tabs)) => (@id(:top), 1, "div", "hbox", "")
end

@merge begin 
  ix_name in enumerate(keys(world.state))
  ix = ix_name[1]
  name = ix_name[2]
  return node(@id(:tabs, ix)) => (@id(:tabs), ix, "button", "", string(name))
end

@merge begin
  return node(@id(:cells)) => (@id(:top), 2, "div", "vbox", "")
end

@merge begin
  displaying() => name
  columns = world[Symbol(name)].columns
  r in 0:length(columns[1])
  return node(@id(:cells, r)) => (@id(:cells), r, "div", "hbox", "")
end

@merge begin
  displaying() => name
  columns = world[Symbol(name)].columns
  c in 1:length(columns)
  column = columns[c]
  r in 1:length(column)
  value = column[r]
  # style = "height: 2em; flex: $(100/length(columns))%"
  # onclick = (c > world[Symbol(name)].num_keys) ? @event(editing() => (name, c, r, string(value))) : ""
  return node(@id(:cells, r, c)) => (@id(:cells, r), c, "div", "flex1", string(value))
end

@merge begin
  displaying() => name
  editing() => (name, c, r, value)
  columns = world[Symbol(name)].columns
  # style = "height: 2em; flex: $(100/length(columns))%"
  # onkeydown = """
  #   if (event.which == 13) {
  #     Blink.msg('event', {'table': 'editing', 'values': ['$name', $c, $r, this.value]}); 
  #     Blink.msg('event', {'table': 'committed', 'values': [true]}); 
  #     return false;
  #   }
  #   if (event.which == 27) {
  #     Blink.msg('event', {'table': 'editing', 'values': ['', 0, 0, '']});
  #     return false;
  #   } 
  # """
  # cell = textarea(Dict(:style=>style, :rows=>1, :onkeydown=>onkeydown), value)
  return node(@id(:cells, r, c)) => (@id(:cells, r), c, "textarea", "flex1", string(value))
end

@merge begin
  displaying() => name
  columns = world[Symbol(name)].columns
  c in 1:length(columns)
  column = columns[c]
  typ = eltype(column)
  # weight = (c > world[Symbol(name)].num_keys) ? "normal" : "bold"
  # style = "border-bottom: 1px solid #aaa; height: 2em; font-weight: $weight; flex: $(100/length(columns))%"
  # node = Hiccup.div(Dict(:style=>style), string(typ))
  return node(@id(:cells, 0, c)) => (@id(:cells, 0), c, "div", "flex1", string(typ))
end
```

I haven't hooked up events yet so that's all commented out for now. I'll need to create some way of unpacking ids to get the useful data back out.

### 2016 Dec 27

DOM patching seems to be working. The implementation is pretty simple. The server does a little more work than before to compute diffs between old and new node tables:

``` julia
post = Sequence([
  # (level, parent, ix, id, kind, class, text)
  @transient sorted_node(Int64, Id, Int64, Id, String)

  @merge begin
    root = UI.root
    node(id) => (root, ix, kind, _, _)
    return sorted_node(1, root, ix, id, kind)
  end
  
  Fixpoint(
    @merge begin
      sorted_node(level, _, _, parent, _,)
      node(id) => (parent, ix, kind, _, _)
      return sorted_node(level+1, parent, ix, id, kind)
    end
  )
  
  @transient class(Id, String)
  
  @merge begin
    node(id) => (_, _, _, class, _)
    return class(id, class)
  end
  
  @transient text(Id, String)
  
  @merge begin
    node(id) => (_, _, _, _, text)
    return text(id, text)
  end
])

function render(window, old_state, new_state)
  (removed, inserted) = Data.diff(old_state[:sorted_node], new_state[:sorted_node])
  (_, _, _, removed_id, _) = removed
  (_, parent, ix, id, kind) = inserted
  (_, (class_id, class)) = Data.diff(old_state[:class], new_state[:class])
  (_, (text_id, text)) = Data.diff(old_state[:text], new_state[:text])
  @js(window, render($removed_id, $parent, $ix, $id, $kind, $class_id, $class, $text_id, $text))
end

function render(window, state)
  (_, parent, ix, id, kind) = state[:sorted_node].columns
  (class_id, class) = state[:class].columns
  (text_id, text) = state[:text].columns
  @js(window, render($([]), $parent, $ix, $id, $kind, $class_id, $class, $text_id, $text))
end
```

Then the client just rolls through and applies the diffs:

``` js
function render(removed, parent, ix, id, kind, classNameId, className, textContentId, textContent) {
    trash = document.createElement(kind[i]);
    document.getElementById("root").appendChild(trash);
    
    for (var i = removed.length - 1; i >= 0; i--) {
        node = document.getElementById(removed[i]);
        trash.appendChild(node);
    }
    
    for (var i = 0; i < parent.length; i++) {
        node = document.getElementById(id[i]);
        if (node == null) {
            node = document.createElement(kind[i]);
        } else if (node.tagName != kind[i].toUpperCase()) {
            oldNode = node
            node = document.createElement(kind[i]);
            while (oldNode.hasChildNodes()) {
                node.appendChild(oldNode.firstChild);
            }
        }
        node.id = id[i];
        parentNode = document.getElementById(parent[i])
        parentNode.insertBefore(node, parentNode.children[ix[i]]);
    }
    
    for (var i = 0; i < classNameId.length; i++) {
        node = document.getElementById(classNameId[i]);
        node.className = className[i];
    }
    
    for (var i = 0; i < textContentId.length; i++) {
        node = document.getElementById(textContentId[i]);
        if (node.children.length == 0) {
            node.textContent = textContent[i];
        }
    }
    
    trash.remove();
}
```

I make a trash node so that I don't have to distinguish between nodes being removed and nodes being moved - anything that changes at all gets put in the trash where it can be found by the insert loop later.

I can't figure out how to do event handlers nicely without multiple returns, so I implemented that first. A bunch of mindless edits to the compiler later, I can write things like:

``` julia
@merge begin
  displaying() => name
  columns = world[Symbol(name)].columns
  c in 1:length(columns)
  column = columns[c]
  r in 1:length(column)
  value = column[r]
  # style = "height: 2em; flex: $(100/length(columns))%"
  # onclick = (c > world[Symbol(name)].num_keys) ? @event(editing() => (name, c, r, string(value))) : ""
  return node(@id(:cells, r, c)) => (@id(:cells, r), c, "div")
  return class(@id(:cells, r, c)) => "flex1"
  return text(@id(:cells, r, c)) => string(value)
end
```

I'm again running into problems with Blink getting blocked on a non-empty queue. Plagued me for half an hour and then went away again. Going to be tricky to fix if I can't reliably repro.

Got some basic event handlers up, but I'm running into the same old async problem. Eg if I get a click event for "tabs-1", how do I know if it means the current "tabs-1" or one from a previous frame. If I'm not going to switch to a synchronous UI framework then I have to make the events carry semantically meaningful data, which means passing more data back and forth over the wire.

For the moment I'm just ignoring that problem, and I've got the whole table interface otherwise working again.

Blink is blocking again! Why? Damned Heisenbug!

I've also added some watchers for the non-Julia files so that I can iterate quickly. It's the first time I've directly interacted with Julias concurrency primitives. It was pleasantly straightforward.

``` julia
function watch_and_load(window, file)
  load!(window, file)
  @schedule begin
    (waits, _) = open(`inotifywait -m $file`)
    while true
      readline(waits)
      load!(window, file)
    end
  end
end

function window(world)
  window = Window()
  opentools(window)
  watch_and_load(window, "src/Imp.js")
  watch_and_load(window, "src/Imp.css")
  ...
end
```

Had to fix a bug in the relation diff.

Also fixed a couple of missed cases in the client render function:

``` js
    for (var i = 0; i < parent.length; i++) {
        node = document.getElementById(id[i]);
        if (node == null) {
            node = document.createElement(tagName[i]);
        } else if (node.tagName != tagName[i].toUpperCase()) {
            oldNode = node
            node = document.createElement(tagName[i]);
            node.className = oldNode.className;
            while (oldNode.hasChildNodes()) {
                node.appendChild(oldNode.firstChild);
            }
            node.onclick = oldNode.onclick;
            node.onkeydown = oldNode.onkeydown;
        }
        node.id = id[i];
        parentNode = document.getElementById(parent[i])
        parentNode.insertBefore(node, parentNode.children[ix[i]-1]);
    }
```

I got fed up of editing css so I added support for in-place styles like:

``` julia
@merge begin
  displaying() => name
  columns = world[Symbol(name)].columns
  c in 1:length(columns)
  column = columns[c]
  r in 1:length(column)
  value = column[r]
  return node(@id(:cells, r, c)) => (@id(:cells, r), c, "div")
  return style(@id(:cells, r, c), "flex") => "1"
  return style(@id(:cells, r, c), "height") => "1.5em"
  return style(@id(:cells, r, c), "margin-left") => "0.5em"
  return style(@id(:cells, r, c), "margin-right") => "0.5em"
  return text(@id(:cells, r, c)) => string(value)
  return cell(@id(:cells, r, c)) => (r, c, string(value))
  return onclick(@id(:cells, r, c))
end
```

The rendering breaks in hard to reproduce ways, and it took me a while to figure out why. `node.style = oldNode.style` silently doesn't work. It just erases the style of node. The correct incantation is `node.style = oldNode.style.cssText`.

### 2016 Dec 28 

I ported the minesweeper example to the new flow/UI system. It's noticably faster - building the hiccup.jl vdom was the majority of the runtime in the previous version, which is daft.

I also hooked in the table browser so I can poke about inside the minesweeper state.

### 2017 Apr 6

(Been a while. Was working on a parallel project. Details later.)

Having to create ids for every dom node sucks. It's boilerplate. 

Also don't like that because I don't have a way to nest queries, I can't directly represent the nested structure of the dom and instead have to break it up into multiple queries.

Could solve the last problem by introducing nested queries to Imp, and I still want to do something like that at some point to solve the context problem, but for now I can solve both by introducing html templates.

``` julia
[div
  login(session) do
    [input "type"="text" "placeholder"="What should we call you?"]
  end
  
  chat(session) do
    [div
      message(message, text, time) do
        [div
          [span "class"="message-time" time]
          [span "class"="message-text" text]
        ]
      end
    ]
    [input "type"="text" "placeholder"="What do you want to say?"]
  end
]
```

Anything that looks like a datalog clause eg `login(session)` repeats the template inside it for every row, sorted in the order that the variables appear. Nested clauses implicitly join against all their ancestors, uh, which makes this a bad example because there are no joins. Later...

In my other project these get compiled into a bunch of datalog views that generate a relational dom model much like the one I used in imp, but once I had it working I realized that now I have templates it would be much simpler to just read data out of the relations and interpret the templates directly, rather than doing all the fiddly codegen. 

``` julia
# dumb slow version just to get the logic right
# TODO not that

function interpret_value(value, bound_vars)
  string(isa(value, Symbol) ? bound_vars[value] : value)
end

function interpret_node(node::TextNode, bound_vars, data)
  return [interpret_value(node.text, bound_vars)]
end

function interpret_node(node::FixedNode, bound_vars, data)
  attributes = Dict{String, String}()
  for attribute in node.attributes
    attributes[interpret_value(attribute.key, bound_vars)] = interpret_value(attribute.val, bound_vars)
  end
  nodes = vcat([interpret_node(child, bound_vars, data) for child in node.children]...)
  return [Hiccup.Node(node.tag, attributes, nodes)]
end

function interpret_node(node::QueryNode, bound_vars, data)
  nodes = []
  columns = data[node.table].columns
  @assert length(node.vars) == length(columns)
  for r in 1:length(columns[1])
    if all((!(var.name in keys(bound_vars)) || (bound_vars[var.name] == columns[c][r]) for (c, var) in enumerate(node.vars)))
      new_bound_vars = copy(bound_vars)
      for (c, var) in enumerate(node.vars)
        new_bound_vars[var.name] = columns[c][r]
      end
      for child in node.children
        push!(nodes, interpret_node(child, new_bound_vars, data)...)
      end
    end
  end
  return nodes
end
```

Doing the diffing should be fairly easy if I track which dom nodes correspond to which query node + bound_vars. This is kinda similar to how Om has a much easier time diffing immutable data compared to React which needs programmer help. But even better, because we get automatic list keys too.

I think the next thing is to get events hooked up.

The tricky part about events is that they often need to grab values from the dom or the event, or run some js code before sending the event to the server. But they also need to grab values from the server. If I only cared about the latter I could do something like `"onclick" = add_message(next_id, "foo")`. If I only cared about the former I could do soemthing like `"onclick" = "imp.send_event('add_message', [1, this.value])"`. But if I need both? I could add js values to the first like `"onclick" = add_message(next_id, js"this.value")` but that still doesn't allow making decisions on the client side. I could attach the query values to the dom node itself, so the event would be `"onclick" = "imp.send_event('add_message', [this.data.next_id, this.value])"`, but that gets pretty verbose. I could splice server-side values into the js like `"onclick" = "imp.send_event('add_message', [$next_id, this.value])"` but that would create a different function for each dom node, and each function needs to be parsed, compiled and stored. I also don't like the lack of symmetry between query and event syntax.

How expensive are js functions? Every source I can find so far only seems to look at closures, where there is one version of the source code and multiple instantiations. If attaching the functions happens on the client I could maybe create a factory function for each event binding in the template.

```js
factory = function(next_id) {
  function (event) {
    imp.send_event('add_message', [next_id, this.value])
  }
}

node.onclick = factory(5).bind(node)
```

(That's probably wrong because js is crazy, but the idea is there.)

Or I could keep server-side data on the server and just send over some id that identifies the row. That would avoid problems with round-tripping interesting types through js, but it introduces concurrency problems where the client might send the id of a node that no longer exists on the server. Not sure which one is going to be messier.

Is there some 80% solution? Let's look at the most common use-cases:

1. Pressing a button to delete a todo.
2. Submitting the contents of a text box on pressing enter.
3. Submitting and clearing the contents of a text box on pressing enter.

1 doesn't require anything interesting to happen on the client. 2 will spam the server with data if we create an event on every button press, so we really want the client to look at the keypress first. 3 has to both submit a server event and call a js function, which ideally we would like to do without an extra roundtrip.

Splicing stuff into js functions seems like the only thing that will handle all three nicely. We just need to make the syntax cleaner and the avoid the cost of creating many similar functions. 

If we tag specific relations as events then we can create js functions for them and write `"onclick" = "add_message($next_id, this.value)"`.

Avoiding many similar functions seems to *have* a feasible solution, which means I can put it off doing it until it actually becomes a problem. 

Julia parses `"add_message($next_id, this.value)"` as `Expr(:string, "add_message(", :next_id, ", this.value)")` so it will be easy detect in the template parser.

I added support for string interpolation everywhere the template accepts string, so now this a valid template:

``` julia
[div
  login(session) do
    [input "type"="text" "placeholder"="What should we call you?"]
  end
  
  chat(session) do
    [div
      message(message, text, time) do
        [div
          [span "class"="message-time" "time: $time"]
          [span "class"="message-text" text]
        ]
      end
    ]
    next_message(id) do
      [input 
        "type"="text" 
        "placeholder"="What do you want to say?"
        "onkeydown"="if (event.keypress == 13) {new_message($id, this.value)}"
        ]
    end
  end
]
```

But now that I look at it I realize that with my intended diff semantics, this will replace the input node every time the next message id changes. Which would be fine except that that also wipes the contents of the input. It's the same problem for all the approaches I came up with above - the core problem is that we can't change attributes without replacing the node. Maybe I need to be able to move the query inside the node?

``` julia
[input 
  "type"="text" 
  "placeholder"="What do you want to say?"
  next_message(id) do "onkeydown"="if (event.keypress == 13) {new_message($id, this.value)}" end
]
```

I'd have to change the syntax and the interpeter, and it will make diffs a bit more complicated, but it should work.

It would also incidentally add the ability to do stuff like:

``` julia
[button 
  style("funky", k, v) do k=v end 
  "bring the funk"
]
```

Which is not super important, but it was the one thing that the current ui system can do that the templates can't.

Oh, I guess tags too. Let's make tags interpolatable.

``` julia
["div"
  login(session) do
    ["input" "type"="text" "placeholder"="What should we call you?"]
  end
  
  chat(session) do
    ["div"
      message(message, text, time) do
        ["div"
          ["span" "class"="message-time" "time: $time"]
          ["span" "class"="message-text" text]
        ]
      end
    ]
    next_message(id) do
      ["input"
        "type"="text" 
        "placeholder"="What do you want to say?"
        "onkeydown"="if (event.keypress == 13) {new_message($id, this.value)}"
        ]
    end
  end
]
```

A little gross. An actual html parser would be better, but I don't have time to write one and I can't figure out a good way to reuse one while keeping the queries and template together.

Still don't like the asymmetry between queries and events either.

:S

### 2017 Apr 7

Parsing and interpreter needed a bit of tweaking to handle attributes inside queries. Treat them as nodes in their own right now rather than as a field of FixedNode. Get pushed into their parent node.

Decided to require string escaping rather than allowing raw symbols. Makes less typing in the template, makes it really clear when values are moving from the server to the client and makes it clear that values will be converted to strings.

``` julia
[div
  login(session) do
    [input placeholder="What should we call you?"]
  end
  
  chat(session) do
    [div
      message(message, text, time) do
        [div
          [span class="message-time" "time: $time"]
          [span class="message-text" "$text"]
        ]
      end
    ]
    [input
     placeholder="What do you want to say?"
     next_message(id) do
       onkeydown="if (event.keypress == 13) {new_message($id, this.value)}"
     end
     ]
  end
]
```

I feel much better about this. Glad I slept on it.

Let's hook it up.

Wrap the templates in a mutable thing to enable live-coding:

``` julia
type View
  template::Any
  parsed_template::Node
  watchers::Set{Any}
end

function View() 
  template = quote [div] end
  View(template, parse_template(template), Set{Any}())
end

function set_template!(view::View, template)
  view.template = template
  view.parsed_template = parse_template(template)
  for watcher in view.watchers
    watcher()
  end
end

function Flows.watch(watcher, view::View)
  push!(view.watchers, watcher)
end
```

Render the whole template each time and use [diffhtml](https://diffhtml.org/) to patch the dom. 

``` julia
function render(window, view, state)
  root = Hiccup.Node(:div)
  interpret_node(root, view.parsed_template, Dict{Symbol, Any}(:session => "my session"), state)
  @js(window, diff.innerHTML(document.body, $(string(root))))
end

function watch_and_load(window, file)
  load!(window, file)
  @schedule begin
    (waits, _) = open(`inotifywait -me CLOSE_WRITE $file`)
    while true
      readline(waits)
      load!(window, file)
    end
  end
end

function window(world, view)
  window = Window()
  opentools(window)
  load!(window, "src/diffhtml.js")
  # watch_and_load(window, "src/Imp.css")
  # watch_and_load(window, "src/Imp.js")
  sleep(3) # :(
  handle(window, "event") do event
    refresh(world, Symbol(event["table"]), tuple(event["values"]...))
  end
  watch(world) do old_state, new_state
    render(window, view, new_state)
  end
  watch(view) do
    render(window, view, world.state)
  end
  @js(window, document.body.innerHTML = $(string(Hiccup.Node(:div))))
  render(window, view, world.state)
  window
end
```

I checked that changing `next_message` updates the event listener without blowing away the input box.

Next thing is getting events hooked up, and then handling sessions properly.

Client-side we have:

``` js
function imp_event(table) {
    return function () {
        Blink.msg("event", {"table": table, "values": Array.from(arguments)});
        return false;
    }
}
```

Flows get extended with a new kind of relation, which is exactly like a transient except that it can be inserted into from the client:

``` julia
macro event(relation)
  (name, keys, vals) = parse_relation(relation)
  :(Create($(Expr(:quote, name)), [$(map(esc, keys)...)], [$(map(esc, vals)...)], true, true))
end
```

In render we create new event functions for every event:

``` julia
function render(window, view, world)
  for event in world.events
    js(window, Blink.JSString("$event = imp_event(\"$event\")"), callback=false)
  end
  ...
end
```

I repeatedly ran into the same old hangs with Blink. I'll try upgrading to the latest version and see if they continue to plague me.

Here is the chat with working events:

``` julia
world = World()
view = View()

world[:chat] = Relation((["my session"],), 1)

begin 
  set_flow!(world, Sequence([
    @stateful login(String)
    @stateful chat(String)
    @stateful message(Int64) => (String, DateTime)
    @event new_message(String)
    
    @merge begin
      new_message(text)
      @query begin
        message(id) => (_, _)
      end
      return message(1 + length(id)) => (text, now())
    end
  ]))
  set_template!(view, quote
    [div
      login(session) do
        [input placeholder="What should we call you?"]
      end
    
      chat(session) do
        [div
          message(message, text, time) do
            [div
              [span class="message-time" "time: $time"]
              [span class="message-text" "$text"]
            ]
          end
        ]
        [input
          placeholder="What do you want to say?"
          onkeydown="if (event.which == 13) {new_message(this.value); this.value=''}"
        ]
      end
    ]
  end)
end

w = window(world, view)
```

So I need to deal with sessions and then add some css and we're done.

The window setup generates a 'unique' session id and stores it in a relation:

``` julia
function window(world, view)
  window = Window()
  session = string(now()) # TODO uuid
  ...
  refresh(world, :session, tuple(session))
  window
end
```

It also gets passed to every render call where it becomes an in-scope variable for the template:

``` julia
function render(window, view, world, session)
  ...
  interpret_node(root, view.parsed_template, Dict{Symbol, Any}(:session => session), world.state)
  ...
end
```

With sessions we can now handle logging in:

``` julia
set_flow!(world, Sequence([
  UI.pre
  
  @stateful username(String) => String
  @stateful message(Int64) => (String, String, DateTime)
  
  @transient not_logged_in(String)
  
  @event new_login() => (String, String)
  @event new_message() => (String, String)
  
  @merge begin
    new_login() => (session, username)
    return username(session) => username
  end
  
  @merge begin
    new_message() => (session, text)
    @query begin
      message(id) => (_, _)
    end
    return message(1 + length(id)) => (session, text, now())
  end
  
  @merge begin
    session(session)
    @query username(session) => un # TODO hygiene bug :(
    @when length(un) == 0
    return not_logged_in(session)
  end
]))
set_template!(view, quote
  [div
    not_logged_in(session) do
      [input 
        placeholder="What should we call you?"
        onkeydown="if (event.which == 13) {new_login('$session', this.value)}"
        ]
    end
  
    username(session, _) do
      [div
        [span "$username"]
        message(message, message_session, text, time) do
          [div
            username(message_session, message_username) do
              [span class="message-username" "$message_username"]
            end
            [span class="message-time" "time: $time"]
            [span class="message-text" "$text"]
          ]
        end
      ]
      [input
        placeholder="What do you want to say?"
        onkeydown="if (event.which == 13) {new_message('$session', this.value); this.value=''}"
      ]
    end
  ]
end)
```

Uh, I realized that I'm handling string slicing poorly. I want `repr` in events but `string` inside attributes and text. That's why the session in the events is in single quotes. I can work around it for now, but need to think about it more carefully later.

Also I ran into a scoping/hygiene bug in subqueries that I had forgotten about. And I also forget that I don't have working negation anymore. The todo list grows faster and faster.

I can add css by just creating css nodes in the head and the diffing works fine. Styling inline works with existing attributes too.

``` julia
set_head!(view, quote
  [style
    "type"="text/css"
    """
    .vbox {
      display: flex;
      flex-direction: column;
    }
    
    .vbox * {
      flex: 1 1 auto;
    }
    
    .hbox {
      display: flex;
      flex-direction: row;
    }
    
    .hbox * {
      flex: 1 1 auto;
    }
    """]
end)
set_body!(view, quote
  [div
    not_logged_in(session) do
      [div 
        class="hbox"
        [input 
          style="margin: 50vh 30vw;"
          placeholder="What should we call you?"
          onkeydown="if (event.which == 13) {new_login('$session', this.value)}"
        ]
      ]
    end
  
    username(session, username) do
      [div 
        class="vbox"
        style="height: 80vh; width: 80vw; margin: 10vh 10vw;"
        [div 
          style="height: 100%; overflow: scroll;"
          [table
            style="width: 100%;"
            message(message, message_session, text, time) do
              [tr
                username(message_session, message_username) do
                  [td style="font-weight: bold" "$message_username:"]
                end
                [td style="width: 100%" "$text"]
                [td "$time"]
              ]
            end
          ]
        ]
        [input
          style="width: 100%; height: 2em"
          placeholder="What do you want to say?"
          onkeydown="if (event.which == 13) {new_message('$session', this.value); this.value=''}"
        ] 
      ]
    end
  ]
end)
```

A nice touch for complex styles would be to concatenate multiple attributes.

``` julia
function interpret_node(parent, node::AttributeNode, bound_vars, state)
  key = interpret_value(node.key, bound_vars)
  val = interpret_value(node.val, bound_vars)
  parent.attrs[key] = string(get(parent.attrs, key, ""), val) 
end
```

So everything is pretty now. What next?

Would be nice to scrollIntoView on new elements. Can we fit that into the existing event system?

Not without the cooperation of the dom patching. At some point I'll have to replace diffhtml with my own thing, and when I do that I can implement synthetic events like onmount. 

Not sure what to do next. Ideas:

* Port over the other examples
* Get Blink attaching to a real browser rather than electron
* Write the diff algorithm

## 2017 Apr 13

Today I'm trying to write a simple betting exchange in each of Imp, Eve and Logicblox. Each one gets two hours.

The Imp version just barely works and exposed a bunch of bugs. I definitely need to rethink the syntax and the flow system.

``` julia
set_flow!(world, Sequence([
  UI.pre

  @stateful order(id::Order) => (time::DateTime, price::Dec64, quantity::Int64, side::Side)
  @stateful matched(buy::Order, sell::Order) => (price::Dec64, quantity::Int64)
  
  @event new_order(price::String, quantity::String, side::String)
  
  @merge begin
    new_order(price_string, quantity_string, side_string)
    time = now()
    price = parse(Dec64, price_string)
    quantity = parse(Int64, quantity_string)
    side = @match side_string begin
      "buy" => Buy
      "sell" => Sell
    end
    @query order(id) => (_,_,_,_)
    return order(1+length(id)) => (time, price, quantity, side)
  end
  
  @transient remaining(side::Side, price::Dec64, time::DateTime, id::Order) => quantity::Int64
  
  Fixpoint(Sequence([
    @clear remaining
  
    @merge begin
      order(order) => (time, price, quantity, side)
      @query matched(order, matched_buy) => (_, bought_quantity)
      @query matched(matched_sell, order) => (_, sold_quantity)
      remaining = quantity - reduce(+, 0, bought_quantity) - reduce(+, 0, sold_quantity)
      @when remaining > 0
      return remaining(side, price, time, order) => remaining
    end
    
    @merge begin
      @query remaining(Buy, buy_price, buy_time, buy_order) => buy_quantity
      @query remaining(Sell, sell_price, sell_time, sell_order) => sell_quantity
      @when length(buy_order) > 0
      @when length(sell_order) > 0
      b = length(buy_order) # max
      s = 1 # min
      @when buy_price[b] >= sell_price[s]
      price = (buy_time[b] < sell_time[s]) ? buy_price[b] : sell_price[s]
      quantity = min(buy_quantity[b], sell_quantity[s])
      return matched(buy_order[b], sell_order[s]) => (price, quantity)
    end
  ]))
  
  @transient to_buy(price::Dec64) => (printed_price::String, quantity::Int64)
  
  @merge begin
    remaining(Buy, price, _, _) => _
    @query remaining(Buy, price, time, order) => quantity
    printed_price = @sprintf("%.4f", Float64(Dec64(price)))
    return to_buy(price) => (printed_price, sum(quantity))
  end
  
  @transient to_sell(neg_price::Dec64) => (printed_price::String, quantity::Int64)
  
  @merge begin
    remaining(Sell, price, _, _) => _
    @query remaining(Sell, price, time, order) => quantity
    printed_price = @sprintf("%.4f", Float64(Dec64(price)))
    return to_sell(-price) => (printed_price, sum(quantity))
  end
]))

set_body!(view, quote
  [div
    [table
      to_buy(_, price, quantity) do
        [tr [td "$price"] [td "$quantity"]]
      end
      [tr
        [td [input placeholder="price"]]
        [td [input placeholder="quantity"]]
        onkeydown="if (event.which == 13) {new_order(this.children[0].children[0].value, this.children[1].children[0].value, 'buy')}"
      ]
      [tr
        [td [input placeholder="price"]]
        [td [input placeholder="quantity"]]
        onkeydown="if (event.which == 13) {new_order(this.children[0].children[0].value, this.children[1].children[0].value, 'sell')}"
      ]
      to_sell(_, price, quantity) do
        [tr [td "$price"] [td "$quantity"]]
      end 
    ]
  ]
end)
```
