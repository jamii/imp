#![feature(proc_macro)]
#![feature(conservative_impl_trait)]

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

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

use regex::Regex;

use std::collections::BTreeMap;
use std::iter::Iterator;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
struct Entity {
    avs: Vec<(Attribute, Value)>,
}

type Attribute = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
enum Value {
    Bool(bool),
    Integer(i64),
    String(String),
    Entity(Entity),
}

impl From<bool> for Value {
    fn from(bool: bool) -> Value {
        Value::Bool(bool)
    }
}

impl From<i64> for Value {
    fn from(integer: i64) -> Value {
        Value::Integer(integer)
    }
}

impl From<String> for Value {
    fn from(string: String) -> Value {
        Value::String(string)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(string: &'a str) -> Value {
        Value::String(string.to_owned())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Value::Bool(bool) => bool.fmt(f),
            &Value::Integer(integer) => integer.fmt(f),
            &Value::String(ref string) => string.fmt(f),
            &Value::Entity(ref entity) => write!(f, "{:?}", entity),
        }
    }
}

impl Value {
    fn as_str(&self) -> Option<&str> {
        match self {
            &Value::String(ref this) => Some(this),
            _ => None,
        }
    }

    fn as_i64(&self) -> Option<i64> {
        match self {
            &Value::Integer(this) => Some(this),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            &Value::Bool(this) => Some(this),
            _ => None,
        }
    }
}

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self {
            &Value::Bool(ref this) if this == other => true,
            _ => false,
        }
    }
}

impl PartialEq<i64> for Value {
    fn eq(&self, other: &i64) -> bool {
        match self {
            &Value::Integer(ref this) if this == other => true,
            _ => false,
        }
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        match self {
            &Value::String(ref this) if this == other => true,
            _ => false,
        }
    }
}

impl PartialEq<Entity> for Value {
    fn eq(&self, other: &Entity) -> bool {
        match self {
            &Value::Entity(ref this) if this == other => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
struct Bag {
    eavs: BTreeMap<(Entity, Attribute), Value>,
}

impl Bag {
    fn new() -> Self {
        Bag { eavs: BTreeMap::new() }
    }

    fn create<A>(&mut self, avs: Vec<(A, Value)>) -> Entity
    where
        A: Into<Attribute>
    {
        let avs = avs.into_iter()
            .map(|(a, v)| (a.into(), v))
            .collect::<Vec<_>>();
        let entity = Entity { avs: avs.clone() };
        for (attribute, value) in avs {
            self.insert(entity.clone(), attribute, value);
        }
        entity
    }

    fn insert<A, V>(&mut self, entity: Entity, attribute: A, value: V)
    where
        A: Into<Attribute>,
        V: Into<Value>,
    {
        self.eavs.insert((entity, attribute.into()), value.into());
    }

    fn find_ea<E: ?Sized, A: ?Sized>(&self, entity: &E, attribute: &A) -> Option<&Value>
    where
        Entity: PartialEq<E>,
        Attribute: PartialEq<A>,
    {
        self.eavs
            .iter()
            .filter(|&(&(ref e, ref a), _)| (e == entity) && (a == attribute))
            .map(|(_, v)| v)
            .next()
    }

    fn find_av<'a, A: ?Sized, V: ?Sized>(
        &'a self,
        attribute: &'a A,
        value: &'a V,
    ) -> impl Iterator<Item = &Entity> + 'a
    where
        Attribute: PartialEq<A>,
        Value: PartialEq<V>,
    {
        self.eavs
            .iter()
            .filter(move |&(&(_, ref a), v)| (a == attribute) && (v == value))
            .map(move |(&(ref e, _), _)| e)
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum Event {
    Search(String),
    New(String),
    Finish(usize, String),
    Edit(usize),
}

fn send(event: Event) -> String {
    format!("es.send('{}')", json!(event))
}

#[derive(Debug, Serialize, Deserialize)]
enum Command {
    Render(String),
}

fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
    sender
        .send_message(&OwnedMessage::Text(json!(c).to_string()))
        .unwrap()
}

fn render(bag: &Bag) -> String {
    (html!{
        div.notes {
            @let search = bag.find_ea(bag.find_av("global", "search").next().unwrap(), "text").unwrap().as_str().unwrap();
            input.search tabindex="0" onkeyup="if (event.which == 13 && event.ctrlKey) { send({New: this.value}); this.value='' } else { send({Search: this.value}) }" value=(search)
            @for (i, note) in bag.find_av("kind", "note").enumerate().collect::<Vec<_>>().into_iter().rev() {
                @let text = bag.find_ea(note, "text").unwrap().as_str().unwrap();
                @if Regex::new(r"\S").unwrap().is_match(&text) {
                    @if bag.find_ea(note, "editing").unwrap().as_bool().unwrap() {
                        div.edit id={"note-" (i)} tabindex="0" autofocus=(true) contenteditable=(true) onblur={"send({Finish: [" (i) ", this.innerText]})"} (text)
                    } @else if text.contains(search) {
                        div.note id={"note-" (i)} tabindex="0" onfocus={"send({Edit:" (i) "})"} ({
                            let mut unsafe_html = String::new();
                            let parser = Parser::new(text);
                            html::push_html(&mut unsafe_html, parser);
                            PreEscaped(unsafe_html)
                        })
                    }
                }
            }
        }
    }).into_string()
}

fn load() -> Bag {
    let mut file = File::open("/home/jamie/imp.db").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let eavs:Vec<((Entity, Attribute), Value)> = serde_json::from_str(&*contents).unwrap();
    Bag{eavs: eavs.into_iter().collect()}
}

fn save(bag: &Bag) {
    let mut file = File::create("/home/jamie/imp.db").unwrap();
    write!(file, "{}", json!(bag.eavs.clone().into_iter().collect::<Vec<_>>())).unwrap();
}

fn main() {
    let bag = Arc::new(Mutex::new(load()));

    let server = Server::bind("127.0.0.1:8080").unwrap();

    for request in server.filter_map(Result::ok) {
        let bag = bag.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

            send_command(&mut sender, Command::Render(render(&bag.lock().unwrap())));

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
                        let event: Event = serde_json::from_str(text).unwrap();
                        let mut bag = bag.lock().unwrap();
                        match event {
                            Event::Search(text) => {
                                let search = bag.create(vec![("global", "search".into())]).clone();
                                bag.insert(search, "text", text);
                            }
                            Event::New(text) => {
                                let max_i = bag.find_av("kind", "note").map(|e| bag.find_ea(e, "i").unwrap().as_i64().unwrap()).max().unwrap();
                                let note = bag.create(vec![("kind", "note".into()), ("i", (max_i + 1).into())]);
                                bag.insert(note.clone(), "text", text);
                                bag.insert(note.clone(), "editing", false);
                                let search = bag.create(vec![("global", "search".into())]).clone();
                                bag.insert(search, "text", "");
                            }
                            Event::Finish(i, text) => {
                                let ii = &(i as i64);
                                let note = bag.find_av("i", ii).next().unwrap().clone();
                                bag.insert(note.clone(), "text", text);
                                bag.insert(note, "editing", false);
                            }
                            Event::Edit(i) => {
                                let ii = &(i as i64);
                                let note = bag.find_av("i", ii).next().unwrap().clone();
                                bag.insert(note, "editing", true);
                            }
                        }
                        send_command(&mut sender, Command::Render(render(&bag)));
                        save(&*bag);
                    }
                    _ => {
                        panic!("A weird message! {:?}", message);
                    }
                }
            }
        });
    }
}
