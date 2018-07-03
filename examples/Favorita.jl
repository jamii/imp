# module Favorita

# using Imp

using JuliaDB

# @time begin
#     holidays_events = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv")
#     items = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv")
#     oil = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv")
#     stores = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv")
#     test = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv")
#     train = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train-mini.csv")
#     transactions = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv")
# end

# @time begin
#     reindex(holidays_events, :date)
#     reindex(items, :item_nbr)
#     reindex(transactions, (:date, :store_nbr))
# end

# @time begin
#     save(holidays_events, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
#     save(items, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
#     save(oil, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
#     save(stores, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
#     save(test, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
#     save(train, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train-mini.jdb")
#     save(transactions, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
# end

# @time begin
#     holidays_events = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
#     items = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
#     oil = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
#     stores = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
#     test = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
#     train = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train-mini.jdb")
#     transactions = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
# end

function get_cofactors(holidays_events, items, oil, stores, test, train, transactions)

    t_date = train.columns.columns[2]::Vector{Date}
    t_store_nbr = train.columns.columns[3]::Vector{Int64}
    t_item_nbr = train.columns.columns[4]::Vector{Int64}
    t_unit_sales = train.columns.columns[5]::Vector{Float64}

    he_date = holidays_events.columns.columns[1]::Vector{Date}
    he_type = holidays_events.columns.columns[2]::PooledArrays.PooledArray{String,UInt8,1,Array{UInt8,1}}

    i_item_nbr = items.columns.columns[1]::Vector{Int64}
    i_family = items.columns.columns[2]::PooledArrays.PooledArray{String,UInt8,1,Array{UInt8,1}}

    store_nbr_store_nbr = Dict{Tuple{Int64, Int64}, Float64}()

    store_nbr_item_nbr = Dict{Tuple{Int64, Int64}, Float64}()
    item_nbr_item_nbr = Dict{Tuple{Int64, Int64}, Float64}()

    store_nbr_unit_sales = Dict{Tuple{Int64, Void}, Float64}()
    item_nbr_unit_sales = Dict{Tuple{Int64, Void}, Float64}()
    unit_sales_unit_sales = Dict{Tuple{Void, Void}, Float64}()

    store_nbr_type = Dict{Tuple{Int64, String}, Float64}()
    item_nbr_type = Dict{Tuple{Int64, String}, Float64}()
    unit_sales_type = Dict{Tuple{Void, String}, Float64}()
    type_type = Dict{Tuple{String, String}, Float64}()

    store_nbr_family = Dict{Tuple{Int64, String}, Float64}()
    item_nbr_family = Dict{Tuple{Int64, String}, Float64}()
    unit_sales_family = Dict{Tuple{Void, String}, Float64}()
    type_family = Dict{Tuple{String, String}, Float64}()
    family_family = Dict{Tuple{String, String}, Float64}()

    for i in 1:(length(train)::Int64)
        store_nbr = t_store_nbr[i]
        item_nbr = t_item_nbr[i]
        unit_sales = t_unit_sales[i]
        
        store_nbr_store_nbr[(store_nbr, store_nbr)] = get(store_nbr_store_nbr, (store_nbr, store_nbr), 0.0) + (1.0 * 1.0)

        store_nbr_item_nbr[(store_nbr, item_nbr)] = get(store_nbr_item_nbr, (store_nbr, item_nbr), 0.0) + (1.0 * 1.0)
        item_nbr_item_nbr[(item_nbr, item_nbr)] = get(item_nbr_item_nbr, (item_nbr, item_nbr), 0.0) + (1.0 * 1.0)
        
        store_nbr_unit_sales[(store_nbr, nothing)] = get(store_nbr_item_nbr, (store_nbr, nothing), 0.0) + (1.0 * unit_sales)
        item_nbr_unit_sales[(item_nbr, nothing)] = get(item_nbr_item_nbr, (item_nbr, nothing), 0.0) + (1.0 * unit_sales)
        unit_sales_unit_sales[(nothing, nothing)] = get(unit_sales_unit_sales, (nothing, nothing), 0.0) + (unit_sales * unit_sales)

        # hes = searchsorted(he_date, t_date[i])
        # for he1 in hes
        #     store_nbr_type[(store_nbr, he_type[he1])] = get(store_nbr_type, (store_nbr, he_type[he1]), 0.0) + (1.0 * 1.0)
        #     item_nbr_type[(item_nbr, he_type[he1])] = get(item_nbr_type, (item_nbr, he_type[he1]), 0.0) + (1.0 * 1.0)
        #     unit_sales_type[(nothing, he_type[he1])] = get(unit_sales_type, (nothing, he_type[he1]), 0.0) + (unit_sales * 1.0)
        
        #     for he2 in hes
        #         type_type[(he_type[he1], he_type[he2])] = get(type_type, (he_type[he1], he_type[he2]), 0.0) + (1.0 * 1.0)
        #     end
        # end

        # i1 = searchsortedfirst(i_item_nbr, item_nbr)
        # @assert i1 <= length(i_family)
        # store_nbr_family[(store_nbr, i_family[i1])] = get(store_nbr_family, (store_nbr, i_family[i1]), 0.0) + (1.0 * 1.0)
        # item_nbr_family[(item_nbr, i_family[i1])] = get(item_nbr_family, (item_nbr, i_family[i1]), 0.0) + (1.0 * 1.0)
        # unit_sales_family[(nothing, i_family[i1])] = get(unit_sales_family, (nothing, i_family[i1]), 0.0) + (unit_sales * 1.0)
        # for he1 in hes
        #     type_family[(he_type[he1], i_family[i1])] = get(type_family, (he_type[he1], i_family[i1]), 0.0) + (1.0 * 1.0)
        # end
        # family_family[(i_family[i1], i_family[i1])] = get(type_family, (i_family[i1], i_family[i1]), 0.0) + (1.0 * 1.0)

        
    end

    return (
        store_nbr_store_nbr,
        store_nbr_item_nbr,
        item_nbr_item_nbr,
        store_nbr_unit_sales,
        item_nbr_unit_sales,
        unit_sales_unit_sales,
        store_nbr_type,
        item_nbr_type,
        unit_sales_type,
        type_type,
        store_nbr_family,
        item_nbr_family,
        unit_sales_family,
        type_family,
        family_family,
    )

end

@time get_cofactors(holidays_events, items, oil, stores, test, train, transactions)

# unique_holidays_events = groupreduce((a,b) -> a, holidays_events, :date)

# data = train
# @time data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
# @show data

using DataFrames
using CSV
using Dates

# Categorical = CSV.CategoricalArrays.CategoricalString{UInt32}
# # Categorical = String

# @time begin
#     holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, Categorical, Categorical, Categorical, Categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
#     items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, Categorical, Int64, Bool], truestring="1", falsestring="0")
#     oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
#     stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, Categorical, Categorical, Categorical, Int8])
#     test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int8, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    train2 = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train-mini.csv", types=[Int64, Date, Int8, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
#     transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int8, Int64], dateformat="yyyy-mm-dd")
# end

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

# @time begin
#     categorical!(data, [:store_nbr, :item_nbr])
# end

# @time begin
#     data = data[data[:unit_sales] .>= 0, :] # scoring function blows up on returns
#     srand(42)
#     sample = rand(Float64, nrow(data)) .< 0.5
#     train_data = data[sample, :]
#     test_data = data[!sample, :]
# end

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

@time model = fit(LinearMixedModel, @formula(unit_sales ~ (1|store_nbr) + (1|item_nbr)), train2)
# # "At a superficial level these can be considered as the "estimates" of the random effects, with a bit of hand waving, but pursuing this analogy too far usually results in confusion."
# params = ranef(model, named=true)

# function StatsBase.fitted(m::LinearMixedModel{T}, new_data) where T
#     new_m = LinearMixedModel(m.formula, new_data)
#     v = Array{T}(nobs(new_m))
#     # TODO will the levels for both be the same?
#     trms = new_m.trms
#     A_mul_B!(vec(v), trms[end - 1], fixef(m))
#     b = ranef(m)
#     for j in eachindex(b)
#         MixedModels.unscaledre!(vec(v), trms[j], b[j])
#     end
#     v
# end

# @time begin
#     actual = test_data[:unit_sales]
#     predicted = fitted(model, data)[!sample] # for some reason LinearMixedModel(formula, test_data) fails
#     predicted = map(x -> max(x, 0.0), predicted)
#     weight = map(p -> p ? 1.25 : 1.00, test_data[:perishable])
#     score = sqrt(sum(@. weight * (log(actual + 1) - log(predicted + 1)) ^ 2) / sum(weight))
# end

# @show score

# using MultivariateStats

# end
