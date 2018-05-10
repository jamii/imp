module Titanic

using Imp
using CSV
using DataFrames

# --- data import ---
# NOTE needs DataStreams master

df = CSV.read("data/titanic/train.csv", header=1, types=[Int64, Int64, Int64, String, String, Union{Float64,Missing}, Int64, Int64, String, Float64, Union{String,Missing}, Union{String,Missing}])
df.colindex
train = Set()
for row in DataFrames.eachrow(df)
    push!(train, tuple((val for (_,val) in row)...))
end
Imp.global_env[Imp.Var(:train)] = train

# --- normalization ---

@imp passenger = p -> exists(train(p))

@imp survival = (p, x) -> exists(train(p, x))

@imp pclass = (p, x) -> exists(train(p, _, x))

@imp name = (p, x) -> exists(train(p, _, _, x))

@imp sex = (p, x) -> exists(train(p, _, _, _, x))

@imp age = (p, x) -> exists(train(p, _, _, _, _, x))

@imp sibsp = (p, x) -> exists(train(p, _, _, _, _, _, x))

@imp parch = (p, x) -> exists(train(p, _, _, _, _, _, _, x))

@imp ticket = (p, x) -> exists(train(p, _, _, _, _, _, _, _, x))

@imp fare = (p, x) -> exists(train(p, _, _, _, _, _, _, _, _, x))

@imp cabin = (p, x) -> exists(train(p, _, _, _, _, _, _, _, _, _, x))

@imp embarked = (p, x) -> exists(train(p, _, _, _, _, _, _, _, _, _, _, x))

end
