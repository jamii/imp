# module Favorita

# using Imp

using JuliaDB

# @time begin
#     holidays_events = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv")
#     items = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv")
#     oil = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv")
#     stores = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv")
#     test = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv")
#     train = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv")
#     transactions = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv")
# end

# @time begin
#     save(holidays_events, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
#     save(items, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
#     save(oil, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
#     save(stores, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
#     save(test, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
#     save(train, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
#     save(transactions, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
# end

# @time begin
#     holidays_events = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
#     items = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
#     oil = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
#     stores = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
#     test = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
#     train = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
#     transactions = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
# end

# unique_holidays_events = groupreduce((a,b) -> a, holidays_events, :date)

# data = train
# @time data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
# @show data

using DataFrames
using CSV
# using Dates

Categorical = CSV.CategoricalArrays.CategoricalString{UInt32}
# Categorical = String

@time begin
    holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, Categorical, Categorical, Categorical, Categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
    items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, Categorical, Int64, Bool], truestring="1", falsestring="0")
    oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
    stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, Categorical, Categorical, Categorical, Int8])
    test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int8, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    train = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv", types=[Int64, Date, Int8, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int8, Int64], dateformat="yyyy-mm-dd")
end

# @time begin
#     unique_holidays_events = by(holidays_events, :date, x -> x[1, 2:end])

#     data = train
#     data = join(data, unique_holidays_events, on=[:date], kind=:left)
#     data = join(data, items, on=[:item_nbr])
#     data = join(data, oil, on=[:date], kind=:left)
#     data = join(data, stores, on=[:store_nbr], makeunique=true)
#     data = join(data, transactions, on=[:date, :store_nbr], kind=:left)
#     @assert size(data)[1] == size(train)[1]
# end

# using Query

# data = @from t in train begin
#     @left_outer_join h in holidays_events on t.date equals h.date
#     @select {t.date}
#     @collect DataFrame
# end

@time begin
    categorical!(data, [:store_nbr, :item_nbr])
end

@time begin
    data = data[data[:unit_sales] .>= 0, :] # scoring function blows up on returns
    srand(42)
    sample = rand(Float64, nrow(data)) .< 0.5
    train_data = data[sample, :]
    test_data = data[!sample, :]
end

# using GLM

# @time begin
#     # TODO what does glm do with missing?
#     model = lm(@formula(unit_sales ~ store_nbr + item_nbr), train_data)
# end

# actual = test_data[:unit_sales]
# redicted = predict(model, test_data)
# weight = map(p -> p ? 1.25 : 1.00, test_data[:perishable])
# score = sqrt(sum(@. weight * (log(actual + 1) - log(predicted + 1)) ^ 2) / sum(weight))

# using FixedEffectModels

# mixed_data = deepcopy(data)
# mixed_data[:unit_sales] = Union{Float64, Missing}[s ? d : missing for (s,d) in zip(sample, mixed_data[:unit_sales])]
# model = reg(mixed_data, @model(unit_sales ~ transactions, fe = store_nbr + item_nbr, save=true))
# @show fes(model)

# @show model.augmentdf

# @time begin
#     actual = test_data[:unit_sales]
#     predicted = predict(model, test_data)
#     weight = map(p -> p ? 1.25 : 1.00, test_data[:perishable])
#     score = sqrt(sum(@. weight * (log(actual + 1) - log(predicted + 1)) ^ 2) / sum(weight))
# end

# using OnlineStats

using MixedModels

@time model = fit(LinearMixedModel, @formula(unit_sales ~ (1|store_nbr) + (1|item_nbr)), train_data)
# "At a superficial level these can be considered as the "estimates" of the random effects, with a bit of hand waving, but pursuing this analogy too far usually results in confusion."
params = ranef(model, named=true)

function StatsBase.fitted(m::LinearMixedModel{T}, new_data) where T
    new_m = LinearMixedModel(m.formula, new_data)
    v = Array{T}(nobs(new_m))
    # TODO will the levels for both be the same?
    trms = new_m.trms
    A_mul_B!(vec(v), trms[end - 1], fixef(m))
    b = ranef(m)
    for j in eachindex(b)
        MixedModels.unscaledre!(vec(v), trms[j], b[j])
    end
    v
end

@time begin
    actual = test_data[:unit_sales]
    predicted = fitted(model, data)[!sample] # for some reason LinearMixedModel(formula, test_data) fails
    predicted = map(x -> max(x, 0.0), predicted)
    weight = map(p -> p ? 1.25 : 1.00, test_data[:perishable])
    score = sqrt(sum(@. weight * (log(actual + 1) - log(predicted + 1)) ^ 2) / sum(weight))
end

@show score

# using MultivariateStats

# end
