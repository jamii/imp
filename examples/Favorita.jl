module Favorita

using ..Imp
# using JuliaDB
using DataFrames
using CSV
using Dates
# using Query
# using MixedModels

using BenchmarkTools

# categorical = CSV.CategoricalArrays.CategoricalString{UInt32}
function df_load(categorical=String)
     holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, categorical, categorical, categorical, categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
    items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, categorical, Int64, Bool], truestring="1", falsestring="0")
    oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
    stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, categorical, categorical, categorical, Int64])
    test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int64, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    train = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv", types=[Int64, Date, Int64, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
    transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int64, Int64], dateformat="yyyy-mm-dd")

    # TODO bring Missing back when https://github.com/JuliaLang/julia/issues/28076 is fixed
    DataFrames.columns(oil)[2] = Float64[ismissing(v) ? NaN : v for v in DataFrames.columns(oil)[2]]
    DataFrames.columns(test)[5] = Int8[ismissing(v) ? 3 : Int8(v) for v in DataFrames.columns(test)[5]]
    DataFrames.columns(train)[6] = Int8[ismissing(v) ? 3 : Int8(v) for v in DataFrames.columns(train)[6]]

    (holidays_events=holidays_events, items=items, oil=oil, stores=stores, test=test, train=train, transactions=transactions)
end

function jdb_load_csv()
    holidays_events = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv")
    items = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv")
    oil = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv")
    stores = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv")
    test = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv")
    train = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv")
    transactions = loadtable("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv")


    save(holidays_events, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
    save(items, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
    save(oil, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
    save(stores, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
    save(test, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
    save(train, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
    save(transactions, "/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")

    nothing
end

function jdb_load()
    holidays_events = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.jdb")
    items = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.jdb")
    oil = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.jdb")
    stores = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.jdb")
    test = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.jdb")
    train = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.jdb")
    transactions = load("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.jdb")

    (holidays_events=holidays_events, items=items, oil=oil, stores=stores, test=test, train=train, transactions=transactions)
end


# NOTE mutates dataframe - dont export outside examples
Imp.Finger(dataframe::DataFrames.DataFrame) = Imp.Finger(dataframe, 1:length(DataFrames.columns(dataframe)))
function Imp.Finger(dataframe::DataFrames.DataFrame, ixes)
    columns = tuple(DataFrames.columns(dataframe)[ixes]...)
    Imp.quicksort!(columns)
    Imp.Finger(columns)
end

function imp_load(db)
    train = Imp.Finger(db.train, [3,4,2,1,5,6])
    stores = Imp.Finger(db.stores)
    items = Imp.Finger(db.items)
    transactions = Imp.Finger(db.transactions, [2,1,3])
    (train=train, stores=stores, items=items, transactions=transactions)
end

function df_join_items(db)
    # unique_holidays_events = by(holidays_events, :date, x -> x[1, 2:end])
    result = db.train
    # result = join(result, unique_holidays_events, on=[:date], kind=:left)
    result = join(result, db.items, on=[:item_nbr])
    @assert size(result)[1] == size(db.train)[1]
    result
end

function jdb_join_items(db)
    # unique_holidays_events = groupreduce((a,b) -> a, holidays_events, :date)
    result = db.train
    result = join(result, db.items, lkey=:item_nbr, rkey=:item_nbr)
    @assert length(result) == length(train)
    result
end

# function q_join_items(db)
#     data = @from t in db.train begin
#         # @left_outer_join h in holidays_events on t.date equals h.date
#         @join i in db.items on t.item_nbr equals i.item_nbr
#         @select {t.id, t.date, t.store_nbr, t.item_nbr, t.unit_sales, t.onpromotion, i.family, i.class, i.perishable}
#         @collect DataFrame
#     end
# end

function df_join(db)
    # unique_holidays_events = by(holidays_events, :date, x -> x[1, 2:end])
    result = db.train
    # result = join(result, unique_holidays_events, on=[:date], kind=:left)
    result = join(result, db.stores, on=[:store_nbr])
    result = join(result, db.items, on=[:item_nbr])
    result = join(result, db.transactions, on=[:date, :store_nbr])
    @assert size(result)[1] == size(db.train)[1]
    result
end

function imp_join(db)
    fingers = (db.train, db.stores, db.items, db.transactions)
    join =
        Imp.GenericJoin(((1,1),(2,1),(4,1)), # store_nbr
        Imp.GenericJoin(((1,2),(3,1)), # item_nbr
        Imp.GenericJoin(((1,3),(4,2)), # date
        Imp.Product((1,4),
        Imp.Product((2,2),
        Imp.Product((3,2),
        Imp.Product((4,3),
        Imp.Done())))))))
    output = Imp.Select((
        (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
        (2,2),(2,3),(2,4),(2,5),
        (3,2),(3,3),(3,4),
        (4,3),
    ))
    result = Imp.run(output, join, fingers)
    @show length(result[1]) length(db.train.columns[1])
    # TODO default values
    # @assert length(result[1]) == length(db.train.columns[1])
    result
end

function imp_count(db)
    fingers = (db.train, db.stores, db.items, db.transactions)
    join =
        Imp.GenericJoin(((1,1),(2,1),(4,1)), # store_nbr
        Imp.GenericJoin(((1,2),(3,1)), # item_nbr
        Imp.GenericJoin(((1,3),(4,2)), # date
        Imp.Product((1,4),
        Imp.Product((2,2),
        Imp.Product((3,2),
        Imp.Product((4,3),
        Imp.Done())))))))
    output = Imp.Count()
    result = Imp.run(output, join, fingers)
    @show result length(db.train.columns[1])
    # TODO default values
    # @assert length(result[1]) == length(db.train.columns[1])
    result
end

function prepare_data(data)
    categorical!(data, [:store_nbr, :item_nbr])
end

function split_data(data)
    data = data[data[:unit_sales] .>= 0, :] # scoring function blows up on returns
    srand(42)
    sample = rand(Float64, nrow(data)) .< 0.5
    train_data = data[sample, :]
    test_data = data[!sample, :]
    (train_data, test_data)
end

# function mixed_fit(data)
#     model = fit(LinearMixedModel, @formula(unit_sales ~ (1|store_nbr) + (1|item_nbr)), train_data)
#     # @show params = ranef(model, named=true)
#     model
# end

# # TODO this is probably not correct
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

function score(test_data, predicted)
    # TODO scoring function breaks on negative predictions
    test_data[:unit_sales]
    predicted = map(x -> max(x, 0.0), predicted)
    weight = map(p -> p ? 1.25 : 1.00, test_data[:perishable])
    score = sqrt(sum(@. weight * (log(actual + 1) - log(predicted + 1)) ^ 2) / sum(weight))
end

function cofactor(ks...)
    Dict{Tuple{ks...}, Float64}()
end

function add!(cofactor::Dict, k1, k2, v)
    k = (k1,k2)
    cofactor[k] = get(cofactor, k, 0.0) + v
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

    store_nbr_store_nbr = cofactor(Int64, Int64)

    store_nbr_item_nbr = cofactor(Int64, Int64)
    item_nbr_item_nbr = cofactor(Int64, Int64)

    store_nbr_unit_sales = cofactor(Int64, Void)
    item_nbr_unit_sales = cofactor(Int64, Void)
    unit_sales_unit_sales = cofactor(Void, Void)

    store_nbr_type = cofactor(Int64, String)
    item_nbr_type = cofactor(Int64, String)
    unit_sales_type = cofactor(Void, String)
    type_type = cofactor(String, String)

    store_nbr_family = cofactor(Int64, String)
    item_nbr_family = cofactor(Int64, String)
    unit_sales_family = cofactor(Void, String)
    type_family = cofactor(String, String)
    family_family = cofactor(String, String)

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

        hes = searchsorted(he_date, t_date[i])
        for he1 in hes
            add!(store_nbr_type, store_nbr, he_type[he1], 1.0 * 1.0)
            add!(item_nbr_type, item_nbr, he_type[he1], 1.0 * 1.0)
            add!(unit_sales_type, nothing, he_type[he1], unit_sales * 1.0)

            for he2 in hes
                add!(type_type, he_type[he1], he_type[he2], 1.0 * 1.0)
            end
        end

        i1 = searchsortedfirst(i_item_nbr, item_nbr)
        @assert 1 <= i1 <= length(i_family)
        add!(store_nbr_family, store_nbr, i_family[i1], 1.0 * 1.0)
        add!(item_nbr_family, item_nbr, i_family[i1], 1.0 * 1.0)
        add!(unit_sales_family, nothing, i_family[i1], unit_sales * 1.0)
        for he1 in hes
            add!(type_family, he_type[he1], i_family[i1], 1.0 * 1.0)
        end
        add!(family_family, i_family[i1], i_family[i1], 1.0 * 1.0)

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

function silly_copy(xs::Vector)
    ys = empty(xs)
    for x in xs
        push!(ys, x)
    end
    ys
end

function silly_copy(xs::Tuple)
    map(silly_copy, xs)
end

macro show_benchmark(b)
    quote
        println($(string(b)))
        # TODO don't understand why nested macros no longer require esc
        display(@benchmark($b))
    end
end

df_db = nothing
jdb_db = nothing
imp_db = nothing

function bench()
    global df_db
    global jdb_db
    global imp_db

    df_db == nothing && (df_db = @time df_load())
    # jdb_db == nothing && (jdb_db = @time jdb_load())
    imp_db == nothing && (imp_db = @time imp_load(df_db))

    @show_benchmark imp_count($imp_db)

    # @show_benchmark df_join_items($df_db)
    # @show_benchmark jdb_join_items($jdb_db)
    # @show_benchmark q_join_items($df_db)
    # @show_benchmark imp_join_items($imp_db)
    # @show_benchmark df_join($df_db)
    @show_benchmark imp_join($imp_db)

    # df_result = df_join_items(df_db)
    # imp_result = imp_join_items(imp_db)
    # @show_benchmark silly_copy($imp_result)

    # @assert Imp.Finger(df_result, [4,1,2,3,5,6,7,8,9]).columns == imp_result
end

end
