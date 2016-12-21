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

world[:displaying] = @relation () => String
world[:editing] = @relation () => (String, Int64, Int64)
world[:edited] = @relation () => (String, Int64, Int64, String)
world[:edit_finished] = @relation () => Bool

world[:cell] = @relation (Int64, Int64) => Hiccup.Node
world[:row] = @relation (Int64,) => Hiccup.Node
world[:tab] = @relation (String,) => Hiccup.Node
world[:window] = @relation () => Hiccup.Node

world[:test] = Relation((Int64[1,2,3], Any[4,5,6]), 1)

fail = []

begin 
  setflow(world, Sequence([
    @create begin 
      edited(name, c, r, value_string)
      columns = world[Symbol(name)].columns
      row = Any[columns[c2][r] for c2 in 1:length(columns)]
      value = try eval(parse(value_string)) catch Exception fail end
      @when !is(value, fail)
      @when typeof(value) <: eltype(columns[c])
      ignore1 = @show (row[c] = value)
      ignore2 = (world.outputs[Symbol(name)] = push!(world[Symbol(name)], tuple(row...)))
      return edit_finished() => true
    end
    
    @merge begin 
      edit_finished() => true
      return editing() => ("", 0, 0)
    end
  
    @create begin 
      name in map(string, keys(world.outputs))
      node = button(Dict(:onclick=>@event displaying() => name), name)
      return tab(name) => node
    end
  
    @create begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      r in 1:length(column)
      value = column[r]
      style = "height: 2em; flex: $(100/length(columns))%"
      onclick = (c > world[Symbol(name)].num_keys) ? @event(editing() => (name, c, r)) : ""
      cell = Hiccup.div(Dict(:style=>style, :onclick=>onclick), render_value(value))
      return cell(c, r) => cell
    end
    
    @merge begin
      displaying() => name
      editing() => (name, c, r)
      columns = world[Symbol(name)].columns
      @when length(columns[1]) >= r
      @query edited(name, c, r, old_edit)
      value = length(old_edit) > 0 ? old_edit[1] : string(columns[c][r])
      style = "height: 2em; flex: $(100/length(columns))%"
      onkeydown = """
        if (event.which == 13) {
          Blink.msg('event', {'table': 'edited', 'values': ['$name', $c, $r, this.value]}); 
          return false;
        }
        if (event.which == 27) {
          Blink.msg('event', {'table': 'editing', 'values': ['', 0, 0]});
          return false;
        } 
      """
      onblur = ""
      cell = textarea(Dict(:style=>style, :rows=>1, :onkeydown=>onkeydown), value)
      return cell(c, r) => cell
    end
    
    @erase edited() => (String, Int64, Int64, String)
    
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
    
    @create begin
      cell(_, r) => _
      @query cell(c, r) => cell_node
      row = hbox(cell_node)
      return row(r) => row
    end
  
    @create begin
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

world.outputs
opentools(w)
# UI.render(w, world.outputs)
# @js w console.log("ok")

# function debug(relation)
#   @relation displaying() => (Int64, String)
#   @relation editing() => (Int64, Int64)
#   @relation contents(String, Int64) => String
#   @relation edited() => (Int64, Int64, String)
#   @relation finished() => (Int64, Int64)
#   
#   @query return relation("displaying") => displaying
#   @query return relation("editing") => editing
#   @query return relation("contents") => contents
#   @query return relation("edited") => edited
#   @query return relation("finished") => finished
#   
#   @query return displaying() => (0, relation[1][1])
#   
#   @query begin
#     relation(name) => relation
#     c in 1:length(relation)
#     return contents(name, c) => ""
#   end
#   
#   window = @Window(displaying, editing, edited, finished) do window, event_number
#     
#     # on edit, stringify row into contents
#     @query begin
#       editing() => ($event_number, r)
#       displaying() => (_, name)
#       relation(name) => relation
#       c in 1:length(relation)
#       value = string(relation[c][r])
#       return contents(name, c) => value
#     end
#     
#     # on change, copy value into contents
#     @query begin
#       edited() => ($event_number, c, value)
#       displaying() => (_, name)
#       return contents(name, c) => value
#     end
#     
#     # on finish, commit value
#     @query begin
#       finished() => ($event_number, 13)
#       displaying() => (_, name)
#       values = @query contents($name, c) => value
#       row = tuple(map(eval, map(parse, values[3]))...)
#       relation(name) => relation
#       unsafe = push!(relation, row)
#       return editing() => (-1, -1)
#     end
#     
#     tables = @query begin
#       relation(name) => _
#       displayed = @exists displaying() => (_, $name)
#       style = displayed ? "font-weight: bold" : ""
#       node = button(Dict(:onclick => @event(displaying() => name), :style => style), name)
#       return (name::String,) => node::Node
#     end
#     
#     picker = hbox(tables[2])
#     
#     cell = @query begin
#       displaying() => (_, name)
#       relation(name) => relation
#       c in 1:length(relation)
#       column = relation[c]
#       r in 1:length(column)
#       value = column[r]
#       style = "height: 2em; flex: $(100/length(column))%"
#       node = Hiccup.div(Dict(:style=>style, :onclick => @event(editing() => r)), render_value(value))
#       return (c::Int64, r::Int64) => node::Node
#     end
#     
#     @query begin
#       displaying() => (_, name)
#       relation(name) => relation
#       c in 1:length(relation)
#       column = relation[c]
#       typ = eltype(column)
#       style = "border-bottom: 1px solid #aaa; height: 2em; flex: $(100/length(column))%"
#       node = Hiccup.div(Dict(:style=>style), string(typ))
#       return cell(c::Int64, 0::Int64) => node::Node
#     end
#     
#     edit = @query begin
#       displaying() => (_, name)
#       relation(name) => relation
#       contents(name, c) => edited_value
#       column = relation[c]
#       style = "height: 2em; flex: $(100/length(column))%"
#       onkeyup = "Blink.msg('event', {'table': 'edited', 'values': [$c, this.value]})"
#       onkeydown = "Blink.msg('event', {'table': 'finished', 'values': [event.keyCode]})"
#       node = textarea(Dict(:style=>style, :rows=>1, :onkeyup=>onkeyup, :onkeydown=>onkeydown), edited_value)
#       return (c::Int64,) => node::Node
#     end
#     
#     row = @query begin
#       displaying() => (_, name)
#       relation(name) => relation
#       r in 0:length(relation[1])
#       is_editing = @exists editing(_, $r)
#       node = if is_editing
#         hbox(@query(edit(c) => n)[2])
#       else
#         hbox(@query(cell(c, $r) => n)[3])
#       end
#       return (r::Int64,) => node::Node
#     end
#     
#     grid = vbox(row[2])
#     
#     Blink.body!(window, vbox([picker, grid]), fade=false)
#     
#   end
#   
#   # tick_every(window, 1000)
# end
# 
# import Minesweeper
# minesweeper_relations = Minesweeper.run(30, 30, 100)
# relation = Relation((["state", "mine", "mine_count", "cleared", "clicked", "cell", "row", "grid"], collect(minesweeper_relations)), 1)
# debug(relation)

end
