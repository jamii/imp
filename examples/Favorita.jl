module Favorita

# using Imp

using DataFrames
using CSV
using Dates

Categorical = CSV.CategoricalArrays.CategoricalString{UInt32}

holidays_events = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/holidays_events.csv", types=[Date, Categorical, Categorical, Categorical, Categorical, Bool], dateformat="yyyy-mm-dd", truestring="True", falsestring="False")
items = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/items.csv", types=[Int64, Categorical, Int64, Bool], truestring="1", falsestring="0")
oil = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/oil.csv", types=[Date, Union{Missing, Float64}], dateformat="yyyy-mm-dd")
stores = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/stores.csv", types=[Int64, Categorical, Categorical, Categorical, Int8])
test = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/test.csv", types=[Int64, Date, Int8, Int64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
train = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/train.csv", types=[Int64, Date, Int8, Int64, Float64, Union{Bool, Missing}], truestring="True", falsestring="False", dateformat="yyyy-mm-dd")
transactions = CSV.read("/home/jamie/.kaggle/competitions/favorita-grocery-sales-forecasting/transactions.csv", types=[Date, Int8, Int64], dateformat="yyyy-mm-dd")

end
