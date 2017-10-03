module Chat

using Data
using Query
using Flows
using UI

view = View()

const Message = Int64

begin 
  set_flow!(view, Sequence([
    @stateful username(Session) => String
    
    @stateful message(Message)
    @stateful text(Message) => String
    @stateful sent_by(Message) => String
    @stateful sent_at(Message) => DateTime
    
    @stateful likes(username::String, Message) 
    
    @event new_login(Session, username::String)
    @merge begin
      new_login(session, username)
      return username(session) => username
    end
    
    @event new_message(Session, text::String)
    @merge begin
      new_message(session, text)
      username(session) => username
      @query begin
        message(id) => (_, _)
      end
      new_message = 1 + length(id)
      return message(new_message)
      return text(new_message) => text
      return sent_by(new_message) => username
      return sent_at(new_message) => now()
    end
    
    # kind of pointless
    # TODO support @event @stateful likes(username::String, Message) 
    @event new_like(Session, Message)
    @merge begin
      new_like(session, message)
      username(session) => username
      return likes(username, message)
    end
    
    @transient not_logged_in(Session)
    @merge begin
      session(session)
      @query username(session) => un # TODO hygiene bug :(
      @when length(un) == 0 # TODO need syntax sugar for negation
      return not_logged_in(session)
    end
  ]))
  set_template!(view, quote
    [html
      [head
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
          """
        ]
      ]
      [body
        [div
          @query not_logged_in(session) begin
            [div 
              class="hbox"
              [input 
                style="margin: 50vh 30vw;"
                placeholder="What should we call you?"
                onkeydown="if (event.which == 13) {new_login($session, this.value)}"
              ]
            ]
          end
    
          @query username(session) => username begin
            [div 
              class="vbox"
              style="height: 80vh; width: 80vw; margin: 10vh 10vw;"
              [div 
                style="height: 100%; overflow: scroll;"
                [table
                  style="width: 100%;"
                  @query message(message) begin
                    [tr
                      @query sent_by(message) => sent_by begin
                        [td style="font-weight: bold" "$sent_by:"]
                      end
                      @query text(message) => text begin
                        [td style="width: 100%" "$text"]
                      end
                      [td 
                        @query likes(liker, message) begin
                          [div "$liker likes this!"]
                        end
                      ]
                      [td [button "like!" onclick="new_like($session, $message)"]]
                    ]
                  end
                ]
              ]
              [input
                style="width: 100%; height: 2em"
                placeholder="What do you want to say?"
                onkeydown="if (event.which == 13) {new_message($session, this.value); this.value=''}"
              ] 
            ]
          end
        ]
      ]
    ]
  end)
end

end
