module Polynomial

using Data
using Query
using Flows
using BenchmarkTools
using Base.Test

const xx = Relation((collect(1:1000000), collect(1:1000000)), 1)
const yy = Relation((collect(1:1000000), collect(1:1000000)), 1)

function f(xx, yy) 
  @query begin
    xx(i, x)
    yy(i, y)
    z = (x * x) + (y * y) + (3 * x * y)
    return (x::Int64, y::Int64, z::Int64)
  end
end

function bench()
  @show @benchmark f(xx, yy)
end

end
