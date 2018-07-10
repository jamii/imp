# module Favorita

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Base.Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

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
#     save(train, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
#     save(transactions, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
# end

@time begin
    holidays_events = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
    items = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
    oil = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
    stores = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
    test = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
    train = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
    transactions = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")
end

mutable struct Buffer{Sorted, Unsorted}
    sorted::Sorted
    unsorted::Unsorted
end

function Buffer(sorted::NDSparse)
    Buffer(sorted, map(c -> typeof(c)(), columns(sorted)))
end

@generated function add!(buffer::Buffer, k::Tuple, v)
    n = length(k.parameters)
    quote
        $(@splice i in 1:n quote
          push!(buffer.unsorted[$i], k[$i])
          end)
        push!(buffer.unsorted[end], v)
        if length(buffer.unsorted[end]) > length(buffer.sorted)
            merge!(buffer)
        end
        buffer
    end
end

function add!(buffer::Buffer, k1, k2, v)
    push!(buffer.unsorted[1], k1)
    push!(buffer.unsorted[2], k2)
    push!(buffer.unsorted[3], v)
    if length(buffer.unsorted[end]) > length(buffer.sorted)
        merge!(buffer)
    end
    buffer
end

@generated function Base.merge!(buffer::Buffer{Sorted, Unsorted}) where {Sorted, Unsorted <: Tuple}
    n = length(Unsorted.parameters)
    quote
        merge!(buffer.sorted, NDSparse(buffer.unsorted..., agg=+), agg=+)
        $(@splice i in 1:n quote
          empty!(buffer.unsorted[$i])
          end)
        buffer
    end
end

# @generated function factor(ks...)
#     n = length(ks)
#     quote
#         # TODO NDSparse is not inferred
#         sorted = NDSparse($(@splice i in 1:n :(Vector{ks[$i]}())), Vector{Float64}())
#         unsorted = tuple($(@splice i in 1:n :(Vector{ks[$i]}())), Vector{Float64}())
#         Buffer(sorted, unsorted)
#     end
# end

Base.isless(::Void, ::Void) = false

function factor(ks...)
    Dict{Tuple{ks...}, Float64}()
end

function add!(factor::Dict, k1, k2, v)
    k = (k1,k2)
    factor[k] = get(factor, k, 0.0) + v
end

function get_cofactors(holidays_events, items, oil, stores, test, train, transactions)

    t_date = train.columns.columns[2]::Vector{Date}
    t_store_nbr = train.columns.columns[3]::Vector{Int64}
    t_item_nbr = train.columns.columns[4]::Vector{Int64}
    t_unit_sales = train.columns.columns[5]::Vector{Float64}

    he_date = holidays_events.columns.columns[1]::Vector{Date}
    he_type = holidays_events.columns.columns[2]::PooledArrays.PooledArray{String,UInt8,1,Array{UInt8,1}}

    i_item_nbr = items.columns.columns[1]::Vector{Int64}
    i_family = items.columns.columns[2]::PooledArrays.PooledArray{String,UInt8,1,Array{UInt8,1}}

    store_nbr_store_nbr = factor(Int64, Int64)

    store_nbr_item_nbr = factor(Int64, Int64)
    item_nbr_item_nbr = factor(Int64, Int64)

    store_nbr_unit_sales = factor(Int64, Void)
    item_nbr_unit_sales = factor(Int64, Void)
    unit_sales_unit_sales = factor(Void, Void)

    store_nbr_type = factor(Int64, String)
    item_nbr_type = factor(Int64, String)
    unit_sales_type = factor(Void, String)
    type_type = factor(String, String)

    store_nbr_family = factor(Int64, String)
    item_nbr_family = factor(Int64, String)
    unit_sales_family = factor(Void, String)
    type_family = factor(String, String)
    family_family = factor(String, String)

    actually_get_cofactors(holidays_events, items, oil, stores, test, train, transactions,

                           t_date,
                           t_store_nbr,
                           t_item_nbr,
                           t_unit_sales,

                           he_date,
                           he_type,

                           i_item_nbr,
                           i_family,

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

function actually_get_cofactors(holidays_events, items, oil, stores, test, train, transactions,

                           t_date,
                           t_store_nbr,
                           t_item_nbr,
                           t_unit_sales,

                           he_date,
                           he_type,

                           i_item_nbr,
                                i_family,
                                
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

    for i in 1:(length(train)::Int64)
        store_nbr = t_store_nbr[i]
        item_nbr = t_item_nbr[i]
        unit_sales = t_unit_sales[i]
        
        add!(store_nbr_store_nbr, store_nbr, store_nbr, 1.0 * 1.0)

        add!(store_nbr_item_nbr, store_nbr, item_nbr, 1.0 * 1.0)
        add!(item_nbr_item_nbr, item_nbr, item_nbr, 1.0 * 1.0)
        
        add!(store_nbr_unit_sales, store_nbr, nothing, 1.0 * unit_sales)
        add!(item_nbr_unit_sales, item_nbr, nothing, 1.0 * unit_sales)
        add!(unit_sales_unit_sales, nothing, nothing, unit_sales * unit_sales)

        # hes = searchsorted(he_date, t_date[i])
        # for he1 in hes
        #     add!(store_nbr_type, store_nbr, he_type[he1]), 1.0 * 1.0)
        #     add!(item_nbr_type, item_nbr, he_type[he1]), 1.0 * 1.0)
        #     add!(unit_sales_type, nothing, he_type[he1]), unit_sales * 1.0)
        
        #     for he2 in hes
        #         add!(type_type, he_type[he1], he_type[he2]), 1.0 * 1.0)
        #     end
        # end

        # i1 = searchsortedfirst(i_item_nbr, item_nbr)
        # @assert 1 <= i1 <= length(i_family)
        # add!(store_nbr_family, store_nbr, i_family[i1], 1.0 * 1.0)
        # add!(item_nbr_family, item_nbr, i_family[i1], 1.0 * 1.0)
        # add!(unit_sales_family, nothing, i_family[i1], unit_sales * 1.0)
        # for he1 in hes
        #     add!(type_family, he_type[he1], i_family[i1]), 1.0 * 1.0)
        # end
        # add!(family_family, i_family[i1], i_family[i1], 1.0 * 1.0)
        
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

# @time get_cofactors(holidays_events, items, oil, stores, test, train, transactions);

# unique_holidays_events = groupreduce((a,b) -> a, holidays_events, :date)

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     data = join(data, items, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     data = join(data, items, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     data = join(items, data, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     reindex(items, :item_nbr)
#     data = join(data, items, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     reindex(items, :item_nbr)
#     data = join(data, items, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

# @time begin
#     data = train
#     # data = join(data, unique_holidays_events, lkey=:date, rkey=:date, how=:left)
#     reindex(data, :item_nbr)
#     reindex(items, :item_nbr)
#     data = join(data, items, lkey=:item_nbr, rkey=:item_nbr)
#     @assert length(data) == length(train)
# end

using DataFrames
using CSV
using Dates

Categorical = CSV.CategoricalArrays.CategoricalString{UInt32}
Categorical = String

@time begin
    df_holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, Categorical, Categorical, Categorical, Categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
    df_items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, Categorical, Int64, Bool], truestring="1", falsestring="0")
    df_oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
    df_stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, Categorical, Categorical, Categorical, Int64])
    df_test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int64, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    df_train = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv", types=[Int64, Date, Int64, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    df_transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int64, Int64], dateformat="yyyy-mm-dd")
end

# @time begin
#     # unique_holidays_events = by(holidays_events, :date, x -> x[1, 2:end])

#     data = df_train
#     # data = join(data, unique_holidays_events, on=[:date], kind=:left)
#     data = join(data, df_items, on=[:item_nbr])
#     # data = join(data, oil, on=[:date], kind=:left)
#     # data = join(data, stores, on=[:store_nbr], makeunique=true)
#     # data = join(data, transactions, on=[:date, :store_nbr], kind=:left)
#     @assert size(data)[1] == size(df_train)[1]
# end

# using Query

# function query(train, items)
#     data = @from t in train begin
#         # @left_outer_join h in holidays_events on t.date equals h.date
#         @join i in items on t.item_nbr equals i.item_nbr
#         @select {t.id, t.date, t.store_nbr, t.item_nbr, t.unit_sales, t.onpromotion, i.family, i.class, i.perishable}
#         @collect DataFrame
#     end
# end

# # @time q = query(train, items)
# @time q = query(df_train, df_items)
# # @time query()

# @time begin
    # categorical!(train2, [:store_nbr, :item_nbr])
# end

# @time begin
#     data = data[data[:unit_sales] .>= 0, :] # scoring function blows up on returns
#     srand(42)
#     sample = rand(Float64, nrow(data)) .< 0.5
#     train_data = data[sample, :]
#     test_data = data[!sample, :]
# end

train_data = df_train

using MixedModels

@time model = fit(LinearMixedModel, @formula(unit_sales ~ (1|store_nbr) + (1|item_nbr)), train_data)
# # "At a superficial level these can be considered as the "estimates" of the random effects, with a bit of hand waving, but pursuing this analogy too far usually results in confusion."
# @show params = ranef(model, named=true)

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
# v
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

