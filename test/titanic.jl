module Titanic

using Imp
using CSV
using DataFrames

# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic/code

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

@lib xor = {x, y} -> x ? x : y

imp_split(string, delim) = collect(enumerate(split(string, delim)))
Imp.global_lib[Imp.Var(:split)] = Imp.Native(imp_split, (String, String), (Int64, String))

# TODO need to handle functions with multiple returns
# @imp split("a b c", " ")

# --- features ---

@imp title = p -> replace(name(p), "(.*, )|(\\..*)", "")

@imp title_vs_sex = (t, s) -> if title(_, t) & sex(_, s)
    count{p -> title(p, t) & sex(p, s)}
end

@imp rare_title = "Dona" | "Lady" | "the Countess" |"Capt" | "Col" | "Don" | "Dr" | "Major" | "Rev" | "Sir" | "Jonkheer"

@imp canonical_title = ("Mlle", "Miss") | ("Ms", "Miss") | ("Mme", "Mrs") | (rare_title, "Rare Title")

@imp title = p -> xor{p.title.canonical_title, p.title}

@imp title_vs_sex = (t, s) -> if title(_, t) & sex(_, s)
    count{p -> title(p, t) & sex(p, s)}
end

end
