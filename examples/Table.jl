module Table

using Data
using Query
using Flows
using UI
using Match
using Blink

world = World()

world[:test] = Relation((Int64[1,2,3], Any[4,5,6], String["a","b","c"]), 1)

fail = []

begin 
  function post(world)
    Sequence([
    @stateful displaying() => String
    @stateful editing() => (String, Int64, Int64, String)
    @stateful cell(Id) => (Int64, Int64, String)
    
    @merge begin
      keydown(id) => (13, value_string)
      cell(id) => (r, c, _)
      displaying() => name
      columns = world[Symbol(name)].columns
      row = Any[columns[c2][r] for c2 in 1:length(columns)]
      value = try eval(parse(value_string)) catch Exception fail end
      @when !is(value, fail)
      @when typeof(value) <: eltype(columns[c])
      ignore1 = (row[c] = value)
      ignore2 = (world.state[Symbol(name)] = push!(world[Symbol(name)], tuple(row...)))
      return editing() => ("", 0, 0, "")
    end
    
    @merge begin
      keydown(id) => (27, value_string)
      return editing() => ("", 0, 0, "")
    end
    
    @merge begin
      click(id)
      cell(id) => (r, c, value)
      displaying() => name
      @when c > world[Symbol(name)].num_keys
      return editing() => (name, r, c, value)
    end
    
    @merge begin
      root = UI.root
      return node(@id(:top)) => (root, 999, "div")
      return style(@id(:top), "display") => "flex"
      return style(@id(:top), "flex-direction") => "column"
    end
    
    @merge begin
      return node(@id(:tabs)) => (@id(:top), 1, "div")
      return style(@id(:tabs), "display") => "flex"
      return style(@id(:tabs), "flex-direction") => "row"
    end
    
    @transient tab(Id) => String
    
    @merge begin 
      ix_name in enumerate(sort(collect(keys(world.state))))
      ix = ix_name[1]
      name = ix_name[2]
      return node(@id(:tabs, ix)) => (@id(:tabs), ix, "button")
      return style(@id(:tabs, ix), "height") => "2em"
      return style(@id(:tabs, ix), "margin") => "0.5em"
      return text(@id(:tabs, ix)) => string(name)
      return tab(@id(:tabs, ix)) => string(name)
      return onclick(@id(:tabs, ix))
    end
    
    @merge begin 
      click(id)
      tab(id, name)
      return displaying() => name
    end
    
    @merge begin
      return node(@id(:cells)) => (@id(:top), 2, "div")
      return style(@id(:cells), "display") => "flex"
      return style(@id(:cells), "flex-direction") => "column"
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      r in 0:length(columns[1])
      return node(@id(:cells, r)) => (@id(:cells), r+1, "div")
      return style(@id(:cells, r), "display") => "flex"
      return style(@id(:cells, r), "flex-direction") => "row"
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      r in 1:length(column)
      value = column[r]
      return node(@id(:cells, r, c)) => (@id(:cells, r), c, "div")
      return style(@id(:cells, r, c), "flex") => "1"
      return style(@id(:cells, r, c), "height") => "1.5em"
      return style(@id(:cells, r, c), "margin-left") => "0.5em"
      return style(@id(:cells, r, c), "margin-right") => "0.5em"
      return text(@id(:cells, r, c)) => repr(value)
      return cell(@id(:cells, r, c)) => (r, c, string(value))
      return onclick(@id(:cells, r, c))
    end
    
    @merge begin
      displaying() => name
      editing() => (name, r, c, value)
      columns = world[Symbol(name)].columns
      return node(@id(:cells, r, c)) => (@id(:cells, r), c, "input")
      return style(@id(:cells, r, c), "flex") => "1"
      return style(@id(:cells, r, c), "height") => "1.5em"
      return style(@id(:cells, r, c), "margin-left") => "0.5em"
      return style(@id(:cells, r, c), "margin-right") => "0.5em"
      return text(@id(:cells, r, c)) => repr(value)
      return cell(@id(:cells, r, c)) => (r, c, string(value))
      return onkeydown(@id(:cells, r, c))
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      typ = eltype(column)
        return node(@id(:cells, 0, c)) => (@id(:cells, 0), c, "div")
        return style(@id(:cells, 0, c), "flex") => "1"
        return style(@id(:cells, 0, c), "height") => "1.5em"
        return style(@id(:cells, 0, c), "margin-left") => "0.5em"
        return style(@id(:cells, 0, c), "margin-right") => "0.5em"
        return style(@id(:cells, 0, c), "border-bottom") => "1px solid #aaa"
        weight = (c <= world[Symbol(name)].num_keys) ? "bold" : "normal"
        return style(@id(:cells, 0, c), "font-weight") => weight
        return text(@id(:cells, 0, c)) => string(typ)
      end
      ])
    end
  
  setflow(world, Sequence([
    UI.pre
    post(world)
    UI.post
  ]))
end

w = window(world)

world.state
# load!(w, "src/Imp.js")
# load!(w, "src/Imp.css")
# @js w console.log("ok")


end
