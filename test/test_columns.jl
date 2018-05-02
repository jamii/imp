module TestColumns

using Test
using BenchmarkTools
using Random

using Imp

@testset "quicksort!" begin
    for i in 1:10000
        srand(i)
        x = rand(Int, i)
        sx = sort(copy(x))
        Imp.quicksort!((x,))
        @test x == sx
    end
end

@inferred Columns(([1,2,3],["a","b","c"]), false)

function bench()
  srand(999)
  x = rand(Int, 10000)
  @show @benchmark quicksort!((copy($x),))
  
  srand(999)
  y = [string(i) for i in rand(Int, 10000)]
  @show @benchmark quicksort!((copy($y),))
end

end
