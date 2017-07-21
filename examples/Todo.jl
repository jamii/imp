module Todo

using Data
using Query
using Flows
using UI
using Match

view = View()

begin 
  set_flow!(view, Sequence([
    # shared state 
    
    @stateful text(todo::Int64) => text::String
    @stateful completed(todo::Int64) => Bool
    @stateful deleted(todo::Int64) => Bool
    
    # session state 
    
    @stateful current_filter(Session) => filter::String
    @stateful editing(Session) => todo::Int64
    
    # redundant computed state (TODO need @prev)
    
    @transient todo_count() => Int64
    @merge begin
      @query text(todo) => _
      return todo_count() => length(todo)
    end
    
    @transient all_completed() => Bool
    @merge begin
      @query completed(todo) => false
      return all_completed() => (length(todo) == 0)
    end
    
    # event handling
    
    @event benchmark(Int64)
    @merge begin
      benchmark(n)
      todo in 1:n
      return text(todo) => "foo"
      return completed(todo) => false
      return deleted(todo) => false
    end
    
    @merge begin
      session(session)
      @query current_filter(session) => filter
      @when length(filter) == 0
      return current_filter(session) => "All"
    end
    
    @merge begin
      session(session)
      @query editing(session) => todo
      @when length(todo) == 0
      return editing(session) => -1
    end
    
    @event new_todo() => text::String
    @merge begin
      new_todo() => text
      todo_count() => count
      todo = count+1
      return text(todo) => text
      return completed(todo) => false
      return deleted(todo) => false
    end
    
    @event delete_todo(todo::Int64)
    @merge begin
      delete_todo(todo)
      return deleted(todo) => true
    end
    
    @event toggle_all() => Bool # TODO need to handle 0-field relations
    @merge begin
      toggle_all() => true
      all_completed() => all_completed
      completed(todo) => _
      return completed(todo) => !all_completed
    end
    
    @event toggle(todo::Int64)
    @merge begin
      toggle(todo)
      completed(todo) => completed
      return completed(todo) => !completed
    end
    
    @event start_editing(Session, todo::Int64)
    @merge begin
      start_editing(session, todo)
      return editing(session) => todo
    end
    
    @event finish_editing(Session, todo::Int64, text::String)
    @merge begin
      finish_editing(session, todo, text)
      return editing(session) => -1 # TODO allow deletion
      return text(todo) => text
    end
    
    @event escape_editing(Session, todo::Int64)
    @merge begin
      escape_editing(session, todo)
      return editing(session) => -1
    end
    
    @event set_filter(Session, filter::String)
    @merge begin
      set_filter(session, filter)
      return current_filter(session) => filter
    end
    
    @event clear_completed(Bool)
    @merge begin
      clear_completed(true)
      completed(todo) => true
      return deleted(todo) => true
    end
    
    # computed state
    
    @transient todo_count() => Int64
    @merge begin
      @query text(todo) => _
      return todo_count() => length(todo)
    end
    
    @transient all_completed() => Bool
    @merge begin
      @query completed(todo) => false
      return all_completed() => (length(todo) == 0)
    end
    
    @transient visible(Session, todo::Int64)
    @merge begin
      current_filter(session) => "Active"
      completed(todo) => false
      deleted(todo) => false
      return visible(session, todo)
    end
    @merge begin
      current_filter(session) => "Completed"
      completed(todo) => true
      deleted(todo) => false
      return visible(session, todo)
    end
    @merge begin
      current_filter(session) => "All"
      completed(todo) => _
      deleted(todo) => false
      return visible(session, todo)
    end
    
    @transient displaying(Session, todo::Int64)
    @merge begin
      editing(session) => editing
      visible(session, todo) 
      @when todo != editing
      return displaying(session, todo)
    end
    
    @transient all_checked(Bool)
    @merge begin
      all_completed() => true
      return all_checked(true)
    end
    
    @transient checked(todo::Int64)
    @merge begin
      completed(todo) => true
      return checked(todo)
    end
    
    @transient completed_count_text() => String
    @merge begin
      @query begin 
        completed(todo) => false
        deleted(todo) => false
      end
      count = length(todo)
      text = (count == 1) ? "1 item left" : "$count items left"
      return completed_count_text() => text
    end
    
    @transient filter(String)
    @merge begin
      filter in ["Active", "Completed", "All"]
      return filter(filter)
    end
  ]))
  set_template!(view, quote
  [html
    [head 
      [link rel="stylesheet" href="http://todomvc.com/examples/backbone/node_modules/todomvc-app-css/index.css"]
    ]
    [body
      [section 
        class="todoapp"
        [header 
          class="header" 
          [h1 "todos"]
          [input 
            class="new-todo" 
            placeholder="What needs to be done?" 
            "type"="text" 
            onkeydown="if (event.which == 13) {new_todo(this.value); me = this; onrender(function() {me.value=''})}" # TODO figure out a better way to handle these kinds of edge-triggered changes
          ]
        ]
        [section 
          class="main"
          [input 
            class="toggle-all" 
            "type"="checkbox" 
            all_checked(todo) do
              checked="checked"
            end
            onclick="toggle_all(true)"]
          [ul
            class="todo-list"
            visible(session, todo) do
              displaying(session, todo) do
                [li 
                  [div 
                    class="view" 
                    [input 
                      class="toggle" 
                      "type"="checkbox" 
                      checked(todo) do
                        checked="checked"
                      end
                      onclick="toggle($todo)"
                    ] 
                    [label 
                      ondblclick="start_editing('$session', $todo)"
                      text(todo, text) do
                        "$text" 
                      end
                    ]
                    [button class="destroy" onclick="delete_todo($todo)"]
                  ]
                ]
              end
              editing(session, todo) do
                [li
                  class="editing"
                  [input  
                    class="edit"
                    text(todo, text) do
                      defaultValue="$text"
                    end
                    onkeydown="""
                      if (event.which == 13) finish_editing('$session', $todo, this.value)
                      if (event.which == 27) escape_editing('$session', $todo)
                    """
                    onblur="escape_editing('$session', $todo)"
                  ]
                ]
              end
            end
          ]
        ]
        [footer
          class="footer"
          completed_count_text(text) do
            [span class="todo-count" "$text"]
          end
          [ul
            class="filters"
            filter(filter) do
              [li 
                [a 
                  current_filter(session, filter) do
                    class="selected"
                  end 
                  onclick="set_filter('$session', '$filter')" 
                  "$filter"
                ]
              ]
            end 
          ]
          [button class="clear-completed" "Clear completed" onclick="clear_completed(true)"]
        ]
      ]
    ]
  ]
end)
end

end
