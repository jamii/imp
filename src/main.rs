#![feature(proc_macro)]

extern crate maud;
extern crate websocket;
extern crate serde;
#[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;
extern crate regex;

use maud::{html, PreEscaped};
use pulldown_cmark::{Parser, html};

use serde::Serialize;
use serde_json::Value;

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

use regex::Regex;

#[derive(Serialize, Deserialize)]
struct Note {
    text: String,
    editing: bool,
}

#[derive(Serialize, Deserialize)]
struct State {
    notes: Vec<Note>,
    search: String,
}

fn command<Kind: Serialize, Args: Serialize>(kind: Kind, args: Args) -> OwnedMessage {
    OwnedMessage::Text(
        json!({
        "kind": kind,
        "args": args,
    }).to_string(),
    )
}

fn render(state: &State) -> String {
    (html!{
        div.notes {
            div.search div contenteditable=(true) onkeyup="if (event.which == 13 && event.ctrlKey) { message('new', [this.innerText]); this.innerText='' } else { message('search', [this.innerText])}" (state.search)
            @for (i, note) in state.notes.iter().enumerate().rev() {
                @if note.editing {
                    div.edit contenteditable=(true) onblur={"message('finish', [" (i) ", this.innerText])"} onkeydown={"if (event.which == 13 && event.ctrlKey) message('finish', [" (i) ", this.innerText]); if (event.which == 27) message('escape', [" (i) "]);"} (&note.text)
                } @else if note.text.contains(&*state.search) {
                    div.note onclick={"message('edit', [" (i) "])"} ({
                        let mut unsafe_html = String::new();
                        let parser = Parser::new(note.text.as_str());
                        html::push_html(&mut unsafe_html, parser);
                        PreEscaped(unsafe_html)
                    })
                }
            }
        }
    }).into_string()
}

fn load() -> State {
    let mut file = File::open("/home/jamie/imp.db").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    serde_json::from_str(&*contents).unwrap()
}

fn save(state: &State) {
    let mut file = File::create("/home/jamie/imp.db").unwrap();
    write!(file, "{}", json!(state)).unwrap();
}

fn main() {
    let state = Arc::new(Mutex::new(load()));

    let server = Server::bind("127.0.0.1:8080").unwrap();

    for request in server.filter_map(Result::ok) {
        let state = state.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

            sender
                .send_message(&command("eval", ("console.log('evaled');",)))
                .unwrap();

            sender
                .send_message(&command("render", (render(&*state.lock().unwrap()),)))
                .unwrap();

            for message in receiver.incoming_messages() {
                let message = message.unwrap();

                match message {
                    OwnedMessage::Close(_) => {
                        let message = OwnedMessage::Close(None);
                        sender.send_message(&message).unwrap();
                        println!("Client {} disconnected", ip);
                        return;
                    }
                    OwnedMessage::Ping(ping) => {
                        let message = OwnedMessage::Pong(ping);
                        sender.send_message(&message).unwrap();
                    }
                    OwnedMessage::Text(ref text) => {
                        println!("Received: {}", text);
                        let json: Value = serde_json::from_str(text).unwrap();
                        match json["kind"].as_str().unwrap() {
                            "search" => {
                                let search = json["args"][0].as_str().unwrap();
                                state.lock().unwrap().search = search.to_owned();
                            }
                            "new" => {
                                let new_text = json["args"][0].as_str().unwrap();
                                state.lock().unwrap().notes.push(Note {
                                    text: new_text.to_owned(),
                                    editing: false,
                                });
                                state.lock().unwrap().search = "".to_owned();
                            }
                            "finish" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                let new_text = json["args"][1].as_str().unwrap();
                                if Regex::new(r"\S").unwrap().is_match(new_text) {
                                    state.lock().unwrap().notes[i] = Note {
                                        text: new_text.to_owned(),
                                        editing: false,
                                    };
                                } else {
                                    state.lock().unwrap().notes.remove(i);
                                }
                            }
                            "edit" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                state.lock().unwrap().notes[i].editing = true;
                            }
                            "escape" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                state.lock().unwrap().notes[i].editing = false;
                            }
                            _ => panic!("What is this message? {}", text),
                        }
                        sender
                            .send_message(&command("render", (render(&*state.lock().unwrap()),)))
                            .unwrap();
                        save(&*state.lock().unwrap())
                    }
                    _ => {
                        panic!("A weird message!");
                    }
                }
            }
        });
    }
}