module Titanic

using Imp
using CSV

# NOTE
# needs DataStreams master

train = CSV.read("data/titanic/train.csv", header=1, types=[Int64, Int64, Int64, String, String, Union{Float64,Missing}, Int64, Int64, String, Float64, Union{String,Missing}, Union{String,Missing}])

@imp x = 1

@imp x

end
