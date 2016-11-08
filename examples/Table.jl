module Table

using Data
using Query
using UI
using Match
using Blink
using Hiccup
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

function debug(relation)
  @relation displaying() => (Int64, String)
  @relation editing() => (Int64, Int64)
  @relation contents(String, Int64) => String
  @relation edited() => (Int64, Int64, Int64, String)
  
  @query return relation("displaying") => displaying
  @query return relation("editing") => editing
  @query return relation("contents") => contents
  @query return relation("edited") => edited
  
  @query return displaying() => (0, relation[1][1])
  
  @query begin
    relation(name) => relation
    c in 1:length(relation)
    return contents(name, c) => ""
  end
  
  window = @Window(displaying, editing, edited) do window, event_number
    
    # on edit, stringify row into contents
    @query begin
      editing() => ($event_number, r)
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      value = string(relation[c][r])
      return contents(name, c) => value
    end
    
    # on change, copy value into contents
    @query begin
      edited() => ($event_number, c, r, value)
      displaying() => (_, name)
      return contents(name, c) => value
    end
    
    tables = @query begin
      relation(name) => _
      displayed = @exists displaying() => (_, $name)
      style = displayed ? "font-weight: bold" : ""
      node = button(Dict(:onclick => @event(displaying() => name), :style => style), name)
      return (name::String,) => node::Node
    end
    
    picker = hbox(tables[2])
    
    cell = @query begin
      displaying() => (_, name)
      relation(name) => relation
      contents(name) => contents
      c in 1:length(relation)
      column = relation[c]
      contents(name, c) => edited_value
      r in 1:length(column)
      value = column[r]
      is_editing = @exists editing(_, $r)
      style = "height: 2em"
      onkeyup = "Blink.msg('event', {'table': 'edited', 'values': [$c, $r, this.value]})"
      node = if is_editing
        textarea(Dict(:style=>style, :rows=>1, :onkeyup=>onkeyup), edited_value)
      else
        Hiccup.div(Dict(:style=>style, :onclick => @event(editing() => r)), string(value))
      end
      return (c::Int64, r::Int64) => node::Node
    end
    
    @query begin
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      column = relation[c]
      typ = eltype(column)
      style = "border-bottom: 1px solid #aaa;"
      node = Hiccup.div(Dict(:style=>style), string(typ))
      return cell(c::Int64, 0::Int64) => node::Node
    end
    
    column = @query begin
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      cells = @query cell($c, r) => n
      style = "margin-left: 2em"
      node = Hiccup.div(Dict(:style=>style), vbox(cells[3]))
      return (c::Int64,) => node::Node
    end
    
    grid = hbox(column[2])
    
    Blink.body!(window, vbox([picker, grid]), fade=false)
    
  end
  
  # tick_every(window, 1000)
end

import Minesweeper
minesweeper_relations = Minesweeper.run(30, 30, 100)
relation = Relation((["state", "mine", "mine_count", "cleared", "clicked", "cell", "row", "grid"], collect(minesweeper_relations)), 1)
debug(relation)

end
