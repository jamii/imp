module Chat 

using Data
using Query
using Flows
using UI
using Match

world = World()
view = View()

begin 
  set_flow!(world, Sequence([
    UI.pre
    
    @stateful username(String) => String
    @stateful message(Int64) => (String, String, DateTime)
    
    @transient not_logged_in(String)
    
    @event new_login() => (String, String)
    @event new_message() => (String, String)
    
    @merge begin
      new_login() => (session, username)
      return username(session) => username
    end
    
    @merge begin
      new_message() => (session, text)
      @query begin
        message(id) => (_, _)
      end
      return message(1 + length(id)) => (session, text, now())
    end
    
    @merge begin
      session(session)
      @query username(session) => un # TODO hygiene bug :(
      @when length(un) == 0
      return not_logged_in(session)
    end
  ]))
  set_head!(view, quote
    [style
      "type"="text/css"
      """
      .vbox {
        display: flex;
        flex-direction: column;
      }
      
      .vbox * {
        flex: 1 1 auto;
      }
      
      .hbox {
        display: flex;
        flex-direction: row;
      }
      
      .hbox * {
        flex: 1 1 auto;
      }
      """]
  end)
  set_body!(view, quote
    [div
      not_logged_in(session) do
        [div 
          class="hbox"
          [input 
            style="margin: 50vh 30vw;"
            placeholder="What should we call you?"
            onkeydown="if (event.which == 13) {new_login('$session', this.value)}"
          ]
        ]
      end
    
      username(session, username) do
        [div 
          class="vbox"
          style="height: 80vh; width: 80vw; margin: 10vh 10vw;"
          [div 
            style="height: 100%; overflow: scroll;"
            [table
              style="width: 100%;"
              message(message, message_session, text, time) do
                [tr
                  username(message_session, message_username) do
                    [td style="font-weight: bold" "$message_username:"]
                  end
                  [td style="width: 100%" "$text"]
                  [td "$time"]
                ]
              end
            ]
          ]
          [input
            style="width: 100%; height: 2em"
            placeholder="What do you want to say?"
            onkeydown="if (event.which == 13) {new_message('$session', this.value); this.value=''}"
          ] 
        ]
      end
    ]
  end)
end

w = window(world, view)

end
