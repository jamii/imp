include("src/Data.jl")
include("src/Query.jl")
include("src/UI.jl")

include("examples/JobData.jl")

include("examples/Graph.jl")
include("examples/Chinook.jl")
include("examples/Job.jl")

Graph.test()
Chinook.test()
Job.test()

Graph.bench()
Chinook.bench()
Job.bench()
