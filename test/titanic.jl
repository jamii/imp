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

# --- lib ---

imp_replace(string, regex, replacement) = replace(string, Regex(regex), replacement)
Imp.global_lib[Imp.Var(:replace)] = Imp.Native(imp_replace, (String, String, String), (String,))

Imp.global_lib[Imp.Var(:+)] = Imp.Native(+, (Int64, Int64), (Int64,))

@lib sum = {x} -> reduce(+, 0, x)

@lib count = {x} -> sum{y -> if x(y) 1 end}

# --- features ---

@imp title = p -> replace(name(p), "(.*, )|(\\..*)", "")

@imp table = (t, s, p) -> if title(p, t) & sex(p, s); 1 end
@imp title_vs_sex = (t, s) -> if table(t, s); sum{table(t, s)} end

# TODO need to project out _ for this to work reasonably
# @imp title_vs_sex = (t, s) -> if title(_, t) & sex(_, s); count{p -> title(p, t) & sex(p, s)} end

end
