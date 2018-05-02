module Tests

using Test

@testset "columns" begin
    include("columns.jl")
end

@testset "basic" begin
    include("basic.jl")
end

end
