module Tests

using Test

@testset "columns" begin
    include("test_columns.jl")
end

@testset "basic" begin
    include("basic.jl")
end

end
