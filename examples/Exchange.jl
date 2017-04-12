module Chat 

using Data
using Query
using Flows
using UI
using Match
using DecFP

world = World()
view = View()

typealias Order Int64

@enum Side Buy Sell

begin 
  set_flow!(world, Sequence([
    UI.pre

    @stateful order(id::Order) => (time::DateTime, price::Dec64, quantity::Int64, side::Side)
    @stateful matched(buy::Order, sell::Order) => (price::Dec64, quantity::Int64)
    
    @event new_order(price::String, quantity::String, side::String)
    
    @merge begin
      new_order(price_string, quantity_string, side_string)
      time = now()
      price = parse(Dec64, price_string)
      quantity = parse(Int64, quantity_string)
      side = @match side_string begin
        "buy" => Buy
        "sell" => Sell
      end
      @query order(id) => (_,_,_,_)
      return order(1+length(id)) => (time, price, quantity, side)
    end
    
    @transient remaining(side::Side, price::Dec64, time::DateTime, id::Order) => quantity::Int64
    
    Fixpoint(Sequence([
      @clear remaining
    
      @merge begin
        order(order) => (time, price, quantity, side)
        @query matched(order, matched_buy) => (_, bought_quantity)
        @query matched(matched_sell, order) => (_, sold_quantity)
        remaining = quantity - reduce(+, 0, bought_quantity) - reduce(+, 0, sold_quantity)
        @when remaining > 0
        return remaining(side, price, time, order) => remaining
      end
      
      @merge begin
        @query remaining(Buy, buy_price, buy_time, buy_order) => buy_quantity
        @query remaining(Sell, sell_price, sell_time, sell_order) => sell_quantity
        @when length(buy_order) > 0
        @when length(sell_order) > 0
        b = length(buy_order) # max
        s = 1 # min
        @when buy_price[b] >= sell_price[s]
        price = (buy_time[b] < sell_time[s]) ? buy_price[b] : sell_price[s]
        quantity = min(buy_quantity[b], sell_quantity[s])
        return matched(buy_order[b], sell_order[s]) => (price, quantity)
      end
    ]))
    
    @transient to_buy(price::Dec64) => (printed_price::String, quantity::Int64)
    
    @merge begin
      remaining(Buy, price, _, _) => _
      @query remaining(Buy, price, time, order) => quantity
      printed_price = @sprintf("%.4f", Float64(Dec64(price)))
      return to_buy(price) => (printed_price, sum(quantity))
    end
    
    @transient to_sell(neg_price::Dec64) => (printed_price::String, quantity::Int64)
    
    @merge begin
      remaining(Sell, price, _, _) => _
      @query remaining(Sell, price, time, order) => quantity
      printed_price = @sprintf("%.4f", Float64(Dec64(price)))
      return to_sell(-price) => (printed_price, sum(quantity))
    end
  ]))
  
  # set_head!(view, quote
  # end)
  set_body!(view, quote
    [div
      [table
        to_buy(_, price, quantity) do
          [tr [td "$price"] [td "$quantity"]]
        end
        [tr
          [td [input placeholder="price"]]
          [td [input placeholder="quantity"]]
          onkeydown="if (event.which == 13) {new_order(this.children[0].children[0].value, this.children[1].children[0].value, 'buy')}"
        ]
        [tr
          [td [input placeholder="price"]]
          [td [input placeholder="quantity"]]
          onkeydown="if (event.which == 13) {new_order(this.children[0].children[0].value, this.children[1].children[0].value, 'sell')}"
        ]
        to_sell(_, price, quantity) do
          [tr [td "$price"] [td "$quantity"]]
        end 
      ]
    ]
  end)
end

w = window(world, view)

end
