include("src/Data.jl")
Data.test()
include("src/Hashed.jl")
include("examples/JobData.jl")

include("src/Query.jl")
include("examples/Graph.jl")
Graph.test()
include("examples/Chinook.jl")
Chinook.test()
include("examples/Job.jl")
Job.test()

Job.bench()

# include("src/UI.jl")
# include("examples/Minesweeper.jl")
