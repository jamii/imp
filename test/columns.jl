module TestColumns

using Test
using Random

using Imp.Columns

@testset "quicksort!" begin
    for i in 1:10000
        srand(i)
        x = rand(Int, i)
        sx = sort(copy(x))
        Columns.quicksort!((x,))
        @test x == sx
    end
end

@testset "merge - all keys match" begin
    for i in 1:10000
        srand(i)
        x = unique(rand(1:i, i))
        y = rand(1:i, length(x))
        z = rand(1:i, length(x))
        a = Relation((x,y), 1)
        b = Relation((x,z), 1)
        c = merge(a,b)
        @test c.columns == b.columns
    end
end

@testset "merge - no keys match" begin
    for i in 1:10000
        srand(i)
        x = unique(rand(1:i, i))
        x1 = [i*2 for i in x]
        x2 = [i*2+1 for i in x]
        y = rand(1:i, length(x))
        z = rand(1:i, length(x))
        a = Relation((x1,y), 1)
        b = Relation((x2,z), 1)
        c = merge(a,b)
        @test length(c.columns[1]) == length(x1) + length(x2)
    end
end

@inferred Relation(([1,2,3],["a","b","c"]), 1)

function bench()
  srand(999)
  x = rand(Int, 10000)
  # @show @benchmark quicksort!((copy($x),))
  
  srand(999)
  y = [string(i) for i in rand(Int, 10000)]
  # @show @benchmark quicksort!((copy($y),))
  
  srand(999)
  x = unique(rand(1:10000, 10000))
  y = rand(1:10000, length(x))
  z = rand(1:10000, length(x))
  a = Relation((x,y), 1)
  b = Relation((x,z), 1)
  # @show @benchmark merge($a,$b)
end

end
