include("src/Data.jl")
include("src/Query.jl")
include("src/UI.jl")
include("examples/Graph.jl")
include("examples/Chinook.jl")

Data.test()
Graph.test()
Chinook.test()

Data.bench()
Graph.bench()
Chinook.bench()

include("examples/JobData.jl")
include("examples/Job.jl")
Job.test()
Job.bench()

include("examples/Minesweeper.jl")
