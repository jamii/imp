module Table

using Data
using Query
using Flows
using UI
using Match
using Blink

world = World()

world[:test] = Relation((Int64[1,2,3], Any[4,5,6]), 1)

fail = []

begin 
  setflow(world, Sequence([
    UI.pre
  
    @stateful displaying() => String
    @stateful editing() => (String, Int64, Int64, String)
    @transient committed() => Bool
    
    @merge return displaying() => "test"
    
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
    
    @merge begin
      root = UI.root
      return node(@id(:top)) => (root, 1, "div", "vbox", "")
    end
    
    @merge begin
      return node(@id(:tabs)) => (@id(:top), 1, "div", "hbox", "")
    end
    
    @merge begin 
      ix_name in enumerate(sort(collect(keys(world.state))))
      ix = ix_name[1]
      name = ix_name[2]
      return node(@id(:tabs, ix)) => (@id(:tabs), ix, "button", "", string(name))
    end
    
    @merge begin
      return node(@id(:cells)) => (@id(:top), 2, "div", "vbox", "")
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      r in 0:length(columns[1])
      return node(@id(:cells, r)) => (@id(:cells), r, "div", "hbox", "")
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      r in 1:length(column)
      value = column[r]
      # style = "height: 2em; flex: $(100/length(columns))%"
      # onclick = (c > world[Symbol(name)].num_keys) ? @event(editing() => (name, c, r, string(value))) : ""
      return node(@id(:cells, r, c)) => (@id(:cells, r), c, "div", "flex1", string(value))
    end
    
    @merge begin
      displaying() => name
      editing() => (name, c, r, value)
      columns = world[Symbol(name)].columns
      # style = "height: 2em; flex: $(100/length(columns))%"
      # onkeydown = """
      #   if (event.which == 13) {
      #     Blink.msg('event', {'table': 'editing', 'values': ['$name', $c, $r, this.value]}); 
      #     Blink.msg('event', {'table': 'committed', 'values': [true]}); 
      #     return false;
      #   }
      #   if (event.which == 27) {
      #     Blink.msg('event', {'table': 'editing', 'values': ['', 0, 0, '']});
      #     return false;
      #   } 
      # """
      # cell = textarea(Dict(:style=>style, :rows=>1, :onkeydown=>onkeydown), value)
      return node(@id(:cells, r, c)) => (@id(:cells, r), c, "textarea", "flex1", string(value))
    end
    
    @merge begin
      displaying() => name
      columns = world[Symbol(name)].columns
      c in 1:length(columns)
      column = columns[c]
      typ = eltype(column)
      # weight = (c > world[Symbol(name)].num_keys) ? "normal" : "bold"
      # style = "border-bottom: 1px solid #aaa; height: 2em; font-weight: $weight; flex: $(100/length(columns))%"
      # node = Hiccup.div(Dict(:style=>style), string(typ))
      return node(@id(:cells, 0, c)) => (@id(:cells, 0), c, "div", "flex1", string(typ))
    end
    
    UI.post
  ]))
end

w = window(world)

world.state
# opentools(w)
# UI.render(w, world.state)
load!(w, "src/Imp.js")
load!(w, "src/Imp.css")
# @js w console.log("ok")


end
