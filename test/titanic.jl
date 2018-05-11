module Titanic

using Imp
using CSV
using DataFrames

# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic/code
# TODO add plots

# --- data import ---
# NOTE needs DataStreams master

data = Set()
train = Set()
test = Set()

df = CSV.read(joinpath(@__DIR__, "../data/titanic/train.csv"), header=1, types=[Int64, Int64, Int64, String, String, Union{Float64,Missing}, Int64, Int64, String, Float64, Union{String,Missing}, Union{String,Missing}])
df.colindex
for row in DataFrames.eachrow(df)
    push!(data, tuple((val for (_,val) in row)...))
    push!(train, (row[1][1],))
end

df = CSV.read(joinpath(@__DIR__, "../data/titanic/test.csv"), header=1, types=[Int64, Int64, String, String, Union{Float64,Missing}, Int64, Int64, String, Union{Float64,Missing}, Union{String,Missing}, Union{String,Missing}])
df.colindex
train = Set()
for row in DataFrames.eachrow(df)
    @show row[1] row[1][1]
    push!(data, tuple(row[1][1], missing, (val for (_,val) in row[2:end])...))
    push!(test, (row[1][1],))
end

Imp.global_env[Imp.Var(:data)] = data
Imp.global_env[Imp.Var(:train)] = train
Imp.global_env[Imp.Var(:test)] = test

# --- normalization ---

# TODO make permute user-accessible

@imp passenger = p -> exists(data(p))

@imp survival = (p, x) -> exists(data(p, x))

@imp pclass = (p, x) -> exists(data(p, _, x))

@imp name = (p, x) -> exists(data(p, _, _, x))

@imp sex = (p, x) -> exists(data(p, _, _, _, x))

@imp age = (p, x) -> exists(data(p, _, _, _, _, x))

@imp sibsp = (p, x) -> exists(data(p, _, _, _, _, _, x))

@imp parch = (p, x) -> exists(data(p, _, _, _, _, _, _, x))

@imp ticket = (p, x) -> exists(data(p, _, _, _, _, _, _, _, x))

@imp fare = (p, x) -> exists(data(p, _, _, _, _, _, _, _, _, x))

@imp cabin = (p, x) -> exists(data(p, _, _, _, _, _, _, _, _, _, x))

@imp embarked = (p, x) -> exists(data(p, _, _, _, _, _, _, _, _, _, _, x))

# --- lib ---

imp_replace(string, regex, replacement) = replace(string, Regex(regex), replacement)
Imp.global_lib[Imp.Var(:replace)] = Imp.Native(imp_replace, (String, String, String), (String,))

Imp.global_lib[Imp.Var(:+)] = Imp.Native(+, (Int64, Int64), (Int64,))

@lib sum = {x} -> reduce(+, 0, x)

@lib count = {x} -> sum{y -> if x(y) 1 end}

@lib xor = {x, y} -> x ? x : y

imp_split(string, regex) = collect(enumerate(map(String, split(string, Regex(regex)))))
Imp.global_lib[Imp.Var(:split)] = Imp.Native(imp_split, (String, String), (Int64, String))

Imp.global_lib[Imp.Var(:string_of_int)] = Imp.Native(string, (Int64,), (String,))
Imp.global_lib[Imp.Var(:int_to_string)] = Imp.Native(string, (Int64,), (String,))

Imp.global_lib[Imp.Var(:join)] = Imp.Native(string, (String, String), (String,))

Imp.global_lib[Imp.Var(:<)] = Imp.Primitive(:|, [
Imp.Native(<, (Int64, Int64), ()),
Imp.Native(<, (Float64, Float64), ())
])

Imp.global_lib[Imp.Var(:<=)] = Imp.Primitive(:|, [
Imp.Native(<=, (Int64, Int64), ()),
Imp.Native(<=, (Float64, Float64), ())
])

Imp.global_env[Imp.Var(:missing)] = Set([(missing,)])

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

@imp surname = p -> split(p.name, "[,.]")(1)

@imp count{surname}
@imp count{surname(_)}

@imp fsize = p -> p.sibsp + p.parch

@imp family = p -> p.surname.join("_").join(p.fsize.int_to_string)

# TODO == is a mess
@imp fsized = p -> begin
    if p.fsize <= 1
        "singleton"
    elseif p.fsize <= 4
        "small"
    elseif p.fsize # surprisingly, the expression is not bounded without this
        "large"
    end
end

@imp deck = p -> split(p.cabin, "")(1)

# TODO calculate median?
@imp embarked_vs_fare = (e, f) -> if embarked(_, e) & fare(_, f)
    if !missing(e)
        count{p -> !((62 | 830)(p)) & embarked(p, e) & fare(p, f)}
    end
end

# TODO why don't we need === to match on missing?
@imp embarked(62)
@imp embarked = p -> embarked(p, missing) ? "C" : embarked(p)
@imp embarked(62)

@imp p -> fare(p, missing)

# TODO calculate median?
@imp similar_fares = f -> if fare(_, f)
    count{p -> p.pclass(3) & p.embarked("S") & p.fare(f)}
end

fares = @imp (f, p) -> p.pclass(3) & p.embarked("S") & p.fare(f) & !missing(f)
median(map(first, collect(fares)))

@imp fare = p -> p(1044) ? 8.05 : p.fare
@imp fare(1044)

# TODO mice imputation?
@imp age = p -> missing(p.age) ? 12.0 : p.age

@imp child = p -> p.age < 18.0
@imp adult = p -> 18.0 <= p.age

@imp s -> if (0|1)(s); count{p -> p.survival(s)} end
@imp s -> if (0|1)(s); count{p -> p.child & p.survival(s)} end
@imp s -> if (0|1)(s); count{p -> p.adult & p.survival(s)} end

@imp mother = p -> p.sex("female") & (0 < p.parch) & (18.0 <= p.age) & !(p.title("Miss"))

@imp s -> if (0|1)(s); count{p -> p.mother & p.survival(s)} end

end
