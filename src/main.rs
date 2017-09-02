#![feature(proc_macro)]

extern crate maud;
extern crate websocket;
extern crate serde;
#[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate pulldown_cmark;

use maud::{html, PreEscaped};
use pulldown_cmark::{Parser, html};

use serde::Serialize;
use serde_json::{Value, Error};

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

#[derive(Serialize, Deserialize)]
struct Note {
    text: String,
    editing: bool,
}

fn command<Kind: Serialize, Args: Serialize>(kind: Kind, args: Args) -> OwnedMessage {
    OwnedMessage::Text(
        json!({
        "kind": kind,
        "args": args,
    }).to_string(),
    )
}

fn render(notes: &[Note]) -> String {
    (html!{
        div.notes {
            button onclick="message('new_note', [])" "+"
            @for (i, note) in notes.iter().enumerate() {
                @if note.editing {
                    textarea.edit onblur={"message('finish', [" (i) ", this.value])"} onkeydown={"if (event.which == 13 && event.ctrlKey) message('finish', [" (i) ", this.value]); if (event.which == 27) message('escape', [" (i) "]);"} (note.text)
                } @else {
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

fn load() -> Vec<Note> {
    let mut file = File::open("/home/jamie/imp.db").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    serde_json::from_str(&*contents).unwrap()
}

fn save(notes: &[Note]) {
    let mut file = File::create("/home/jamie/imp.db").unwrap();
    write!(file, "{}", json!(notes)).unwrap();
}

fn main() {
    let notes = Arc::new(Mutex::new(load()));

    let server = Server::bind("127.0.0.1:8080").unwrap();

    for request in server.filter_map(Result::ok) {
        let notes = notes.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

            sender
                .send_message(&command("eval", ("console.log('evaled');",)))
                .unwrap();

            sender
                .send_message(&command("render", (render(&*notes.lock().unwrap()),)))
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
                            "new_note" => {
                                notes.lock().unwrap().push(Note {
                                    text: "New!".to_owned(),
                                    editing: true,
                                })
                            }
                            "finish" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                let new_text = json["args"][1].as_str().unwrap();
                                notes.lock().unwrap()[i] = Note {
                                    text: new_text.to_owned(),
                                    editing: false,
                                };
                            }
                            "edit" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                notes.lock().unwrap()[i].editing = true;
                            }
                            "escape" => {
                                let i = json["args"][0].as_u64().unwrap() as usize;
                                notes.lock().unwrap()[i].editing = false;
                            }
                            _ => panic!("What is this message? {}", text),
                        }
                        sender
                            .send_message(&command("render", (render(&*notes.lock().unwrap()),)))
                            .unwrap();
                        save(&*notes.lock().unwrap())
                    }
                    _ => {
                        panic!("A weird message!");
                    }
                }
            }
        });
    }
}