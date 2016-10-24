module Exchange

using Data
using Query
using UI
using Match
using DecFP
using Blink
using Hiccup
@tags button

bitstype 64 Order
@enum Side Buy Sell

@relation order(Order) => (DateTime, Dec64, Int64, Side)
@relation matched(Order, Order) => (Dec64, Int64)
@relation remaining(Side, Dec64, DateTime, Order) => Int64

@query begin
  @minimum remaining(Buy, buy_price, buy_time, buy_order) => buy_quantity
  @maximum remaining(Sell, sell_price, sell_time, sell_order) => sell_quantity
  @when buy_price >= sell_price
  price = (buy_time < sell_time) ? buy_price : sell_price
  quantity = min(buy_quantity, sell_quantity)
  return matched(buy_order, sell_order) => (price, quantity)
end

@query begin
  order(order) => (time, price, quantity, side)
  bought = @query match(order, matched_order) => (_, matched_quantity)
  sold = @query match(matched_order, order) => (_, matched_quantity)
  remaining = quantity - sum(bought[3]) - sum(sold[3])
  @when remaining > 0
  return remaining(side, price, time, order) => remaining
end

end
