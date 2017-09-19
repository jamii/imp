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
#[macro_use]
extern crate nom;

use maud::{html, PreEscaped};
use pulldown_cmark::{Parser, html};

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

use regex::Regex;

use std::collections::{HashMap, BTreeMap};
use std::iter::Iterator;
use std::cell::Cell;

use std::ops::Range;

use std::error::Error;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub struct Entity {
    avs: Vec<(Attribute, Value)>,
}

pub type Attribute = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    String(String),
    Entity(Entity),
}

impl From<bool> for Value {
    fn from(bool: bool) -> Value {
        Value::Boolean(bool)
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
            &Value::Boolean(bool) => bool.fmt(f),
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
            &Value::Boolean(this) => Some(this),
            _ => None,
        }
    }
}

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self {
            &Value::Boolean(ref this) if this == other => true,
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
pub struct Bag {
    eavs: BTreeMap<(Entity, Attribute), Value>,
}

impl Bag {
    fn new() -> Self {
        Bag { eavs: BTreeMap::new() }
    }

    fn create<A>(&mut self, avs: Vec<(A, Value)>) -> Entity
    where
        A: Into<Attribute>,
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
pub enum Event {
    Search(String),
    New(String),
    Finish(usize, String),
    Edit(usize),
}

fn send(event: Event) -> String {
    format!("es.send('{}')", json!(event))
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Command {
    Render(String),
}

fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
    sender
        .send_message(&OwnedMessage::Text(json!(c).to_string()))
        .unwrap()
}

fn run_query(code: &str, bag: &Bag) -> String {
    match parse(code) {
        Ok(query) => format!("{:?}", compile(&query).solve(bag)),
        Err(e) => format!("{:?}", e),
    }
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
                    div.result (run_query(text, bag))
                }
            }
        }
    }).into_string()
}

fn load() -> Bag {
    let mut file = File::open("/home/jamie/imp.db").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let eavs: Vec<((Entity, Attribute), Value)> = serde_json::from_str(&*contents).unwrap();
    Bag { eavs: eavs.into_iter().collect() }
}

fn save(bag: &Bag) {
    let mut file = File::create("/home/jamie/imp.db").unwrap();
    write!(
        file,
        "{}",
        json!(bag.eavs.clone().into_iter().collect::<Vec<_>>())
    ).unwrap();
}

fn serve() {
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
                                let max_i = bag.find_av("kind", "note")
                                    .map(|e| bag.find_ea(e, "i").unwrap().as_i64().unwrap())
                                    .max()
                                    .unwrap();
                                let note = bag.create(vec![
                                    ("kind", "note".into()),
                                    ("i", (max_i + 1).into()),
                                ]);
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

// f should be |t| t < value or |t| t <= value
pub fn gallop<'a, T, F: Fn(&T) -> bool>(slice: &'a [T], mut lo: usize, hi: usize, f: F) -> usize {
    if lo < hi && f(&slice[lo]) {
        let mut step = 1;
        while lo + step < hi && f(&slice[lo + step]) {
            lo = lo + step;
            step = step << 1;
        }

        step = step >> 1;
        while step > 0 {
            if lo + step < hi && f(&slice[lo + step]) {
                lo = lo + step;
            }
            step = step >> 1;
        }

        lo += 1
    }
    lo
}

// TODO I don't like these cells
#[derive(Clone, Debug)]
pub struct RowDomain {
    columns: [Vec<Value>; 3],
    ranges: [Cell<(usize, usize)>; 4],
    current_column: Cell<usize>,
}

impl RowDomain {
    fn narrow(&self, value: &Value) {
        assert!(self.current_column.get() < 3);
        let column = &self.columns[self.current_column.get()];
        let (old_start, old_end) = self.ranges[self.current_column.get()].get();
        self.current_column.set(self.current_column.get() + 1);
        let start = gallop(column, old_start, old_end, |v| v < value);
        let end = gallop(column, start, old_end, |v| v <= value); // TODO there is an extra comparison here that I cna't seem to get rid off
        // println!("{:?} {:?} {:?} {:?}", column, value, start, end);
        self.ranges[self.current_column.get()].set((start, end));
    }

    fn widen(&self) {
        assert!(self.current_column.get() > 0);
        self.current_column.set(self.current_column.get() - 1);
    }

    fn foreach<F>(&self, mut f: F) -> ()
    where
        F: FnMut(&Value) -> (),
    {
        assert!(self.current_column.get() < 3);
        let column = &self.columns[self.current_column.get()];
        let (old_start, old_end) = self.ranges[self.current_column.get()].get();
        self.current_column.set(self.current_column.get() + 1);
        let mut start = old_start;
        while start < old_end {
            let value = &column[start];
            let end = gallop(column, start + 1, old_end, |v| v <= value);
            self.ranges[self.current_column.get()].set((start, end));
            // println!("{:?}", start..end);
            f(value);
            start = end;
        }
    }

    fn is_empty(&self) -> bool {
        let (start, end) = self.ranges[self.current_column.get()].get();
        start >= end
    }

    fn sample(&self) -> [&Value; 3] {
        [
            &self.columns[0][self.ranges[1].get().0],
            &self.columns[1][self.ranges[2].get().0],
            &self.columns[2][self.ranges[3].get().0],
        ]
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Constant(usize, Value),
    Join(Vec<usize>),
}

impl Constraint {
    fn constrain<F>(&self, row_domains: &[RowDomain], mut f: F) -> ()
    where
        F: FnMut(&[RowDomain]) -> (),
    {
        match self {
            &Constraint::Constant(row, ref value) => {
                let row_domain = &row_domains[row];
                row_domain.narrow(value);
                if !row_domain.is_empty() {
                    f(row_domains);
                }
                row_domain.widen();
            }
            &Constraint::Join(ref rows) => {
                let row = rows[0]; // TODO pick smallest
                let row_domain = &row_domains[row];
                row_domain.foreach(|value| {
                    for &row in &rows[1..] {
                        row_domains[row].narrow(value);
                    }
                    if !rows.iter().any(|row| row_domains[*row].is_empty()) {
                        f(row_domains);
                    }
                    for &row in &rows[1..] {
                        row_domains[row].widen();
                    }
                });
                row_domain.widen();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Query {
    row_orderings: Vec<[usize; 3]>,
    constraints: Vec<Constraint>,
}

impl Query {
    fn solve(&self, bag: &Bag) -> Vec<Value> {
        // TODO strip out this compat layer
        let eavs: Vec<[Value; 3]> = bag.eavs
            .iter()
            .map(|(&(ref e, ref a), v)| {
                [
                    Value::Entity(e.clone()),
                    Value::String(a.clone()),
                    v.clone(),
                ]
            })
            .collect();
        let row_domains: Vec<RowDomain> = self.row_orderings
            .iter()
            .map(|row_ordering| {
                let mut ordered_eavs: Vec<[Value; 3]> = eavs.iter()
                    .map(|eav| {
                        [
                            eav[row_ordering[0]].clone(),
                            eav[row_ordering[1]].clone(),
                            eav[row_ordering[2]].clone(),
                        ]
                    })
                    .collect();
                ordered_eavs.sort_unstable();
                let columns = [
                    ordered_eavs.iter().map(|eav| eav[0].clone()).collect(),
                    ordered_eavs.iter().map(|eav| eav[1].clone()).collect(),
                    ordered_eavs.iter().map(|eav| eav[2].clone()).collect(),
                ];
                let range = Cell::new((0, eavs.len()));
                RowDomain {
                    columns: columns,
                    ranges: [range.clone(), range.clone(), range.clone(), range.clone()],
                    current_column: Cell::new(0),
                }
            })
            .collect();

        let mut results: Vec<Value> = vec![];
        solve_constraints(&*row_domains, &*self.constraints, &mut results);
        results
    }
}

fn solve_constraints(
    row_domains: &[RowDomain],
    constraints: &[Constraint],
    results: &mut Vec<Value>,
) {
    // println!(
    //     "{:?}",
    //     row_domains
    //         .iter()
    //         .map(|row_domain| &row_domain.ranges)
    //         .collect::<Vec<_>>()
    // );
    if constraints.len() == 0 {
        for row_domain in row_domains {
            for &value in &row_domain.sample() {
                results.push(value.clone());
            }
        }
    } else {
        constraints[0].constrain(row_domains, |row_domains| {
            solve_constraints(row_domains, &constraints[1..], results)
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueExpr {
    Constant(Value),
    Variable(String),
}

pub type RowExpr = [ValueExpr; 3];

#[derive(Debug, Clone)]
pub struct QueryExpr {
    rows: Vec<RowExpr>,
}

fn compile(query_expr: &QueryExpr) -> Query {

    // sort variables
    let mut constants: Vec<(&Value, (usize, usize))> = vec![];
    let mut variables: Vec<(&String, Vec<(usize, usize)>)> = vec![];
    for (row, row_expr) in query_expr.rows.iter().enumerate() {
        for (col, value_expr) in row_expr.iter().enumerate() {
            match value_expr {
                &ValueExpr::Constant(ref value) => constants.push((value, (row, col))),
                &ValueExpr::Variable(ref variable) => {
                    match variables.iter().position(|&(ref v, _)| *v == variable) {
                        Some(ix) => variables[ix].1.push((row, col)),
                        None => variables.push((variable, vec![(row, col)])),
                    }
                }
            }
        }
    }

    // create constraints
    let mut constraints = vec![];
    for &(value, (row, col)) in constants.iter() {
        constraints.push(Constraint::Constant(row, value.clone()));
    }
    for &(_, ref rows_and_cols) in variables.iter() {
        constraints.push(Constraint::Join(
            rows_and_cols.iter().map(|&(r, c)| r).collect(),
        ));
    }

    // sort rows
    let mut phase: HashMap<(usize, usize), usize> = HashMap::new();
    for (i, &(_, rc)) in constants.iter().enumerate() {
        phase.insert(rc, i);
    }
    for (i, &(_, ref rcs)) in variables.iter().enumerate() {
        for &rc in rcs.iter() {
            phase.insert(rc, constants.len() + i);
        }
    }
    let row_orderings = query_expr
        .rows
        .iter()
        .enumerate()
        .map(|(r, _)| {
            let mut row_ordering = [0, 1, 2];
            row_ordering.sort_unstable_by(|&c1, &c2| {
                phase.get(&(r, c1)).unwrap().cmp(
                    phase.get(&(r, c2)).unwrap(),
                )
            });
            row_ordering
        })
        .collect();

    Query {
        row_orderings,
        constraints,
    }
}

mod syntax {
    use super::*;
    use nom::*;

    named!(integer(&[u8]) -> i64, map_res!(digit, |b| std::str::from_utf8(b).unwrap().parse::<i64>()));

    named!(boolean(&[u8]) -> bool, map!(alt!(tag!("true") | tag!("false")), |b| b == b"true"));

    named!(bare_string(&[u8]) -> String, map_res!(is_not!(" \t\r\n"), |b| std::str::from_utf8(b).map(|s| s.to_owned())));

    named!(delimited_string(&[u8]) -> String, map_res!(delimited!(char!('"'), not!(char!('"')), char!('"')), |b| std::str::from_utf8(b).map(|s| s.to_owned())));

    named!(value(&[u8]) -> Value, alt!(map!(integer, Value::Integer) | map!(boolean, Value::Boolean) | map!(delimited_string, Value::String) | map!(bare_string, Value::String)));

    named!(variable(&[u8]) -> String, map_res!(tuple!(char!('?'), is_not!(" \t\r\n")), |(_, b)| std::str::from_utf8(b).map(|s| s.to_owned())));

    named!(value_expr(&[u8]) -> ValueExpr, dbg_dmp!(alt!(map!(variable, ValueExpr::Variable) | map!(value, ValueExpr::Constant))));

    named!(row_expr(&[u8]) -> RowExpr, dbg_dmp!(do_parse!(
    v1: value_expr >>
    space >>
    v2: value_expr >>
    space >>
    v3: value_expr >>
    ([v1,v2,v3])
    )));

    named!(pub query_expr(&[u8]) -> QueryExpr, dbg_dmp!(map!(separated_nonempty_list_complete!(tuple!(opt!(space), line_ending), row_expr), |rs| QueryExpr{rows:rs})));
}

#[derive(Debug)]
enum ParseError<'a> {
    NomError(nom::IError<&'a [u8]>),
    Remaining(&'a [u8], QueryExpr)
}

fn parse(code: &str) -> Result<QueryExpr, ParseError> {
    match syntax::query_expr(code.as_bytes()) {
        nom::IResult::Done(remaining, query_expr) => {
            if remaining.len() == 0 {
                Ok(query_expr)
            } else {
                Err(ParseError::Remaining(remaining, query_expr))
            }
        }
        nom::IResult::Error(error) => Err(ParseError::NomError(nom::IError::Error(error))),
        nom::IResult::Incomplete(needed) => Err(ParseError::NomError(nom::IError::Incomplete(needed))),
    }
}

fn run_code(bag: &Bag, code: &str, cursor: i64) -> String {
    let codelets = code.split("\n\n").collect::<Vec<_>>();
    let mut focused = None;
    let mut remaining_cursor = cursor;
    for codelet in codelets {
        remaining_cursor -= codelet.len() as i64;
        if remaining_cursor <= 0 {
            focused = Some(codelet);
            break;
        }
        remaining_cursor -= 2; // \n\n
        if remaining_cursor < 0 {
            focused = None;
            break; 
        }
    }
    if let Some(codelet) = focused {
        match parse(codelet) {
            Ok(query_expr) => {
                let query = compile(&query_expr);
                format!("{}\n\n{:?}\n\n{:?}\n\n{:?}\n\n{:?}", codelet, cursor, query_expr, query, query.solve(&bag))
            }
            Err(error) => {
                format!("{}\n\n{:?}\n\n{:?}", codelet, cursor, error)
            }
        }
    } else {
        format!("Nothing\n\n{:?}", cursor)
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum EditorEvent {
    State(String, i64)
}

fn serve_editor() {
    let bag = Arc::new(Mutex::new(load()));

    let server = Server::bind("127.0.0.1:8081").unwrap();

    for request in server.filter_map(Result::ok) {
        let bag = bag.clone();
        thread::spawn(move || {
            let client = request.accept().unwrap();
            let ip = client.peer_addr().unwrap();
            println!("Connection from {}", ip);

            let (mut receiver, mut sender) = client.split().unwrap();

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
                        let event: EditorEvent = serde_json::from_str(text).unwrap();
                        let mut bag = bag.lock().unwrap();
                        match event {
                            EditorEvent::State(code, cursor) => {
                                let result = run_code(&*bag, &*code, cursor);
                                print!("\x1b[2J\x1b[1;1H");
                                println!("{}\n\n", result);
                            }
                        }
                    }
                    _ => {
                        panic!("A weird message! {:?}", message);
                    }
                }
            }
        });
    }
}

fn main() {
    // serve();

    let bag = load();

    // e.kind = "note"
    let query = Query {
        row_orderings: vec![[1, 2, 0]],
        constraints: vec![
            Constraint::Constant(0, "kind".into()),
            Constraint::Constant(0, "note".into()),
            Constraint::Join(vec![0]),
        ],
    };

    println!("{:?}", query.solve(&bag));

    // e.kind = "note"
    // e.editing = true
    let query = Query {
        row_orderings: vec![[1, 2, 0], [1, 2, 0]],
        constraints: vec![
            Constraint::Constant(0, "kind".into()),
            Constraint::Constant(0, "note".into()),
            Constraint::Constant(1, "editing".into()),
            Constraint::Constant(1, true.into()),
            Constraint::Join(vec![0, 1]),
        ],
    };

    println!("{:?}", query.solve(&bag));

    let query = compile(&QueryExpr {
        rows: vec![
            [
                ValueExpr::Variable("e".into()),
                ValueExpr::Constant("kind".into()),
                ValueExpr::Constant("note".into()),
            ],
            [
                ValueExpr::Variable("e".into()),
                ValueExpr::Constant("editing".into()),
                ValueExpr::Constant(true.into()),
            ],
        ],
    });

    println!("{:?}", query);
    println!("{:?}", query.solve(&bag));

    println!("{:?}", parse("?e kind note\n?e editing true"));

    serve_editor();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let col = (0..1000).collect::<Vec<_>>();
        for i in 0..1000 {
            assert_eq!(col[gallop(&*col, 0, 1000, |&x| x < i)], i);
        }
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x < -1), 0);
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x < 1000), 1000);

        for i in 0..999 {
            assert_eq!(col[gallop(&*col, 0, col.len(), |&x| x <= i)], i + 1);
        }
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x <= -1), 0);
        assert_eq!(gallop(&*col, 0, col.len(), |&x| x <= 999), 1000);
    }
}
