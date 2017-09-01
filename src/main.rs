#![feature(proc_macro)]

extern crate maud;
extern crate websocket;
extern crate serde;
#[macro_use(json, json_internal)]
extern crate serde_json;

use maud::html;

use serde::Serialize;
use serde_json::{Value, Error};

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

fn command<Kind: Serialize, Args: Serialize>(kind: Kind, args: Args) -> OwnedMessage {
    OwnedMessage::Text(
        json!({
        "kind": kind,
        "args": args,
    }).to_string(),
    )
}

fn render(notes: &[String]) -> String {
    (html!{
        div.notes {
            button onclick="message('new_note', [])" "+"
            @for note in notes {
                div.note (note)
            }
        }
    }).into_string()
}

fn main() {
    let notes = Arc::new(Mutex::new(vec!["First post!".to_owned()]));

    let server = Server::bind("127.0.0.1:8080").unwrap();

    for request in server.filter_map(Result::ok) {
        let notes = notes.clone();
        thread::spawn(move || {
            let mut client = request.accept().unwrap();
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
                            "new_note" => notes.lock().unwrap().push("New!".to_owned()),
                            other => panic!("What is this message? {}", text),
                        }
                        sender
                            .send_message(&command("render", (render(&*notes.lock().unwrap()),)))
                            .unwrap();
                    }
                    _ => {
                        panic!("A weird message!");
                    }
                }
            }
        });
    }
}