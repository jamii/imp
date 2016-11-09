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
  @relation edited() => (Int64, Int64, String)
  @relation finished() => (Int64, Int64)
  
  @query return relation("displaying") => displaying
  @query return relation("editing") => editing
  @query return relation("contents") => contents
  @query return relation("edited") => edited
  @query return relation("finished") => finished
  
  @query return displaying() => (0, relation[1][1])
  
  @query begin
    relation(name) => relation
    c in 1:length(relation)
    return contents(name, c) => ""
  end
  
  window = @Window(displaying, editing, edited, finished) do window, event_number
    
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
      edited() => ($event_number, c, value)
      displaying() => (_, name)
      return contents(name, c) => value
    end
    
    # on finish, commit value
    @query begin
      finished() => ($event_number, 13)
      displaying() => (_, name)
      values = @query contents($name, c) => value
      row = tuple(map(eval, map(parse, values[3]))...)
      relation(name) => relation
      unsafe = push!(relation, row)
      return editing() => (-1, -1)
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
      c in 1:length(relation)
      column = relation[c]
      r in 1:length(column)
      value = column[r]
      style = "height: 2em; flex: $(100/length(column))%"
      node = Hiccup.div(Dict(:style=>style, :onclick => @event(editing() => r)), string(value))
      return (c::Int64, r::Int64) => node::Node
    end
    
    @query begin
      displaying() => (_, name)
      relation(name) => relation
      c in 1:length(relation)
      column = relation[c]
      typ = eltype(column)
      style = "border-bottom: 1px solid #aaa; height: 2em; flex: $(100/length(column))%"
      node = Hiccup.div(Dict(:style=>style), string(typ))
      return cell(c::Int64, 0::Int64) => node::Node
    end
    
    edit = @query begin
      displaying() => (_, name)
      relation(name) => relation
      contents(name, c) => edited_value
      column = relation[c]
      style = "height: 2em; flex: $(100/length(column))%"
      onkeyup = "Blink.msg('event', {'table': 'edited', 'values': [$c, this.value]})"
      onkeydown = "Blink.msg('event', {'table': 'finished', 'values': [event.keyCode]})"
      node = textarea(Dict(:style=>style, :rows=>1, :onkeyup=>onkeyup, :onkeydown=>onkeydown), edited_value)
      return (c::Int64,) => node::Node
    end
    
    row = @query begin
      displaying() => (_, name)
      relation(name) => relation
      r in 0:length(relation[1])
      is_editing = @exists editing(_, $r)
      node = if is_editing
        hbox(@query(edit(c) => n)[2])
      else
        hbox(@query(cell(c, $r) => n)[3])
      end
      return (r::Int64,) => node::Node
    end
    
    grid = vbox(row[2])
    
    Blink.body!(window, vbox([picker, grid]), fade=false)
    
  end
  
  # tick_every(window, 1000)
end

import Minesweeper
minesweeper_relations = Minesweeper.run(30, 30, 100)
relation = Relation((["state", "mine", "mine_count", "cleared", "clicked", "cell", "row", "grid"], collect(minesweeper_relations)), 1)
debug(relation)

end
