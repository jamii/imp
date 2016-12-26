module Table

using Data
using Query
using Flows
using UI
using Match
using Hiccup
using Blink
@tags button, textarea

macro exists(clause)
  quote 
    exists = @query begin 
      exists = true
      $clause
      return (exists::Bool,)
    end
    length(exists[1]) == 1
  end
end

macro not(clause)
  quote 
    exists = @query begin 
      exists = true
      $clause
      return (exists::Bool,)
    end
    length(exists[1]) == 0
  end
end

function render_value(value)
  Hiccup.span(string(value))
end

function render_value(value::Node)
  value
end

world = World()

world[:test] = Relation((Int64[1,2,3], Any[4,5,6]), 1)

fail = []

begin 
  setflow(world, Sequence([
    @stateful displaying() => String
    @stateful editing() => (String, Int64, Int64, String)
    @transient committed() => Bool
  
    @merge begin 
      committed() => true
      editing() => (name, c, r, value_string)
      columns = world[Symbol(name)].columns
      row = Any[columns[c2][r] for c2 in 1:length(columns)]
      value = try eval(parse(value_string)) catch Exception fail end
      @when !is(value, fail)
      @when typeof(value) <: eltype(columns[c])
      ignore1 = @show (row[c] = value)
      ignore2 = (world.state[Symbol(name)] = push!(world[Symbol(name)], tuple(row...)))
      return editing() => ("", 0, 0, "")
    end
    
    @transient tab(String,) => Hiccup.Node
  
    @merge begin 
      name in map(string, keys(world.state))
      node = button(Dict(:onclick=>@event displaying() => name), name)
      return tab(name) => node
    end
    
    @transient cell(Int64, Int64) => Hiccup.Node
  
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      r in 1:length(column)
      value = column[r]
      style = "height: 2em; flex: $(100/length(columns))%"
      onclick = (c > world[Symbol(name)].num_keys) ? @event(editing() => (name, c, r, string(value))) : ""
      cell = Hiccup.div(Dict(:style=>style, :onclick=>onclick), render_value(value))
      return cell(c, r) => cell
    end
    
    @merge begin
      displaying() => name
      editing() => (name, c, r, value)
      columns = world[Symbol(name)].columns
      style = "height: 2em; flex: $(100/length(columns))%"
      onkeydown = """
        if (event.which == 13) {
          Blink.msg('event', {'table': 'editing', 'values': ['$name', $c, $r, this.value]}); 
          Blink.msg('event', {'table': 'committed', 'values': [true]}); 
          return false;
        }
        if (event.which == 27) {
          Blink.msg('event', {'table': 'editing', 'values': ['', 0, 0, '']});
          return false;
        } 
      """
      cell = textarea(Dict(:style=>style, :rows=>1, :onkeydown=>onkeydown), value)
      return cell(c, r) => cell
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      typ = eltype(column)
      weight = (c > world[Symbol(name)].num_keys) ? "normal" : "bold"
      style = "border-bottom: 1px solid #aaa; height: 2em; font-weight: $weight; flex: $(100/length(columns))%"
      node = Hiccup.div(Dict(:style=>style), string(typ))
      return cell(c, 0) => node
    end
    
    @transient row(Int64) => Hiccup.Node
    
    @merge begin
      cell(_, r) => _
      @query cell(c, r) => cell_node
      row = hbox(cell_node)
      return row(r) => row
    end
    
    @transient window() => Hiccup.Node
  
    @merge begin
      @query tab(name) => tab_node
      tabs = hbox(tab_node)
      @query row(r) => row_node
      rows = vbox(row_node)
      window = vbox([tabs, rows])
      return window() => window
    end 
  ]))
end

w = window(world)

world.state
opentools(w)
# UI.render(w, world.state)
# @js w console.log("ok")


end
