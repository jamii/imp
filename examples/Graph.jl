module Graph

using Data
using Query
using BenchmarkTools
using Base.Test

srand(999)
const edge1 = Relation(([1, 2, 3, 3, 4], [2, 3, 1, 4, 2]))
const edge2 = Relation((rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6))))
# edge3 = read_columns("/home/jamie/soc-LiveJournal1.txt", [Int32, Int32], comments=true)

function f(edge) 
  @query begin
    edge(a,b)
    @when a < b
    edge(b,c)
    @when b < c
    edge(c,a)
    return (a, b, c)
  end
end

function test()
  @test f(edge1).columns == (
  [1, 2],
  [2, 3],
  [3, 4],
  )
end

function bench()
  @show @benchmark f(edge1)
  @show @benchmark f(edge2)
end

end
