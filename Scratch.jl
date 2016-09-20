include("src/Data.jl")
include("src/Query.jl")
Data.test()
include("src/UI.jl")

include("examples/Graph.jl")
include("examples/Chinook.jl")
Graph.test()
Chinook.test()

include("examples/JobData.jl")
include("examples/Job.jl")
Job.test()

include("examples/Minesweeper.jl")
