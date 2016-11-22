module Graph

using Data
using Query
using Query.@view
using Flows
using BenchmarkTools
using Base.Test

srand(999)
const edge1 = Relation(([1, 2, 3, 3, 4], [2, 3, 1, 4, 2]), 2)
const edge2 = Relation((rand(1:Int64(1E5), Int64(1E6)), rand(1:Int64(1E5), Int64(1E6))), 2)

function f(edge) 
  @query begin
    edge(a,b)
    @when a < b
    edge(b,c)
    @when b < c
    edge(c,a)
    return (a::Int64, b::Int64, c::Int64)
  end
end

tris = @view begin
  edge(a,b)
  @when a < b
  edge(b,c)
  @when b < c
  edge(c,a)
  return (a::Int64, b::Int64, c::Int64)
end

function test()
  @test f(edge1)[1] == [1, 2]
  @test f(edge1)[2] == [2, 3]
  @test f(edge1)[3] == [3, 4]
  
  flow = Flow()
  
  flow[:edge] = edge1
  @test flow[:edge] == edge1
  
  local old, new
  push!(flow.watchers, (old_cached, new_cached) -> (old, new) = (old_cached, new_cached))
  
  setviews(flow, [:tris => tris])
  @test !haskey(old, :tris)
  @test haskey(new, :tris)
  @test flow[:tris][1] == [1, 2]
  @test flow[:tris][2] == [2, 3]
  @test flow[:tris][3] == [3, 4]
end

function bench()
  @show @benchmark f(edge1)
  @show @benchmark f(edge2)
end

end
