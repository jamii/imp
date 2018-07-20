module Favorita

using ..Imp
# using JuliaDB
using DataFrames
using CSV
using Dates
# using Query
# using MixedModels
using Rematch

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
function Imp.Finger(dataframe::DataFrames.DataFrame, ixes; default=nothing)
    columns = tuple(DataFrames.columns(dataframe)[ixes]...)
    Imp.quicksort!(columns)
    Imp.Finger(columns; default=default)
end

function imp_load(db)
    train = Imp.Finger(db.train, [2,3,4,1,5,6])
    stores = Imp.Finger(db.stores)
    items = Imp.Finger(db.items)
    transactions = Imp.Finger(db.transactions, [1,2,3]; default=(Date(1,1,1), 0, 0))
    oil = Imp.Finger(db.oil, [1,2]; default=(Date(1,1,1), 0.0))
    holidays_events = Imp.Finger(db.holidays_events, [1,2,3,4,5,6]; default=(Date(1,1,1), "", "", "", "", false))
    (train=train, stores=stores, items=items, transactions=transactions, oil=oil, holidays_events=holidays_events)
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
    fingers = (db.train, db.stores, db.items, db.transactions, db.oil, db.holidays_events)
    query =
        Imp.GenericJoin((1,4,5,6), # date
        Imp.GenericJoin((1,2,4), # store_nbr
        Imp.GenericJoin((1,3), # item_nbr
        Imp.Product(1,
        Imp.Product(2,
        Imp.Product(3,
        # Imp.Product(4,
        # Imp.Product(5,
        # Imp.Product(6, # TODO skips multiple holidays
        Imp.Select((
        (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
        (2,2),(2,3),(2,4),(2,5),
        (3,2),(3,3),(3,4),
        (4,3),
        (5,2),
        (6,2),(6,3),(6,4),(6,5),(6,6)
    ))))))))
    result = Imp.run(query, fingers)
    @show length(result[1]) length(db.train.columns[1])
    @assert length(result[1]) == length(db.train.columns[1])
    result
end

function imp_count(db)
    fingers = (db.train, db.stores, db.items, db.transactions, db.oil, db.holidays_events)
    query =
        Imp.GenericJoin((1,4,5,6), # date
        Imp.GenericJoin((1,2,4), # store_nbr
        Imp.GenericJoin((1,3), # item_nbr
        Imp.Product(1,
        Imp.Product(2,
        Imp.Product(3,
        # Imp.Product(4,
        # Imp.Product(5,
        # Imp.Product(6, # TODO skips multiple holidays
        Imp.Count()
        ))))))
    result = Imp.run(query, fingers)
    @show result length(db.train.columns[1])
    @assert result == length(db.train.columns[1])
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

@enum CofactorKind Continuous Categorical

function cofactor_key(::Vector{T}, kind::CofactorKind) where T
    if kind == Continuous
        Nothing
    elseif kind == Categorical
        T
    end
end

@inline function cofactor_action(kind::CofactorKind, value)
    if kind == Continuous
        (nothing, Float64(value))
    elseif kind == Categorical
        (value, 1.0)
    end
end

struct Cofactor{c1, c2, K1, K2, T1, T2}
    data::Dict{Tuple{T1, T2}, Float64}
end
Cofactor(c1, c2, K1, K2, T1, T2) = Cofactor{c1, c2, K1, K2, T1, T2}(Dict{Tuple{T1, T2}, Float64}())

@inline function add!(cofactor::Cofactor{c1, c2, K1, K2}, columns, r) where {c1, c2, K1, K2}
    (k1, v1) = cofactor_action(K1, columns[c1][r])
    (k2, v2) = cofactor_action(K2, columns[c2][r])

    k = (k1,k2)
    v = v1 * v2
    data = cofactor.data
    data[k] = get(data, k, 0.0) + v
end

function make_cofactors(columns, kinds)
    cofactors = []
    for i in 1:length(columns)
        for j in 1:length(columns)
            if i <= j
                push!(cofactors, Cofactor(i, j, kinds[i], kinds[j], cofactor_key(columns[i], kinds[i]), cofactor_key(columns[j], kinds[j])))
            end
        end
    end
    tuple(cofactors...)
end

function get_cofactors(columns, kinds)
    actually_get_cofactors(columns, make_cofactors(columns, kinds))
end

@generated function actually_get_cofactors(columns, cofactors)
    quote
        for r in 1:length(columns[1])
            $(@splice j in 1:length(cofactors.parameters) quote
              add!(cofactors[$j], columns, r)
            end)
        end
        cofactors
    end
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

function cache(f, key)
    try
        task_local_storage(key)
    catch
        task_local_storage(key, f())
    end
end

function dump()
    df_db = cache(:df_db) do
        @time df_load()
    end
    imp_db = cache(:imp_db) do
        @time imp_load(df_db)
    end
    result = cache(:result) do
        @time imp_join(imp_db)
    end
    # @time CSV.write("result.csv", df)
    file = open("result.csv", "w")
    for c in 1:length(result)
        print(file, "abcdefghijklmnopqrstuvwxyz"[c])
        print(file, "\t")
    end
    print(file, "\n")
    for r in 1:length(result[1])
        for c in 1:length(result)
            print(file, result[c][r])
            print(file, "\t")
        end
        print(file, "\n")
    end
    close(file)
end

function bench()
    df_db = cache(:df_db) do
        @time df_load()
    end
    # jdb_db = cache(:jdb_db) do
    #     @time jdb_load()
    # end
    imp_db = cache(:imp_db) do
        @time imp_load(df_db)
    end

    # @time imp_count(imp_db)

    # @show_benchmark df_join_items($df_db)
    # @show_benchmark jdb_join_items($jdb_db)
    # @show_benchmark q_join_items($df_db)
    # @show_benchmark imp_join_items($imp_db)
    # @show_benchmark df_join($df_db)
    # @time imp_join(imp_db)

    columns = imp_db.train.columns[[2,3,5]]
    @code_warntype actually_get_cofactors(columns, make_cofactors(columns, [Categorical, Categorical, Continuous]))
    cofactors = @time get_cofactors(columns, [Categorical, Categorical, Continuous])
    @show [length(c.data) for c in cofactors]

    # df_result = df_join_items(df_db)
    # imp_result = imp_join_items(imp_db)
    # @show_benchmark silly_copy($imp_result)

    # @assert Imp.Finger(df_result, [4,1,2,3,5,6,7,8,9]).columns == imp_result

    nothing
end

# function bench2()
#     result = cache(:result) do
#         # db = @time CSV.read("result.csv", delim='\t', types=[Date, Int64, Int64, Int64, Float64, Int64, String, String, String, Int64, String, Int64, Bool, Int64, Float64, String, String, String, String, Bool], allowmissing=:none)
#         # db = @time loadtable("result.csv", output="/home/jamie/tmp", delim='\t')
#         cs = Any[db.columns.columns...]
#         pop!(cs)
#         cs[15] = cs[15].values
#         df = DataFrame(cs, map(Symbol, collect("abcdefghijklmnopqrst")))
#         for i in 1:20
#             if eltype(df.columns[i]) == String
#                 categorical!(df, i)
#             end
#         end
#         df

#         # 2013-01-01 | 25 | 103665 | 0 | 7.0 | 3 | Salinas | Santa Elena | D | 1 | BREAD/BAKERY | 2712 | true | 770 | NaN | Holiday | National | Ecuador | Primer dia del ano | false

#     end

#     model = @time fit(LinearMixedModel, @formula(e ~ (1|a) + (1|b) + (1|c) + (1|f) + (1|g) + (1|h) + (1|i) + (1|j) + (1|k) + (1|l) + (1|m) + (1|n) + o + p + (1|q) + (1|r) + (1|s) + (1|t)), result)
#     # @show params = ranef(model, named=true)
#     model
# end

bench()

end
