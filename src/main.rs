#![feature(proc_macro)]
#![feature(conservative_impl_trait)]

extern crate websocket;
#[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate nom;

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

use std::collections::{HashMap, BTreeMap};
use std::iter::Iterator;

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

type LoHi = (usize, usize);
type RowCol = (usize, usize);

#[derive(Debug, Clone)]
pub enum Constraint {
    Constant(RowCol, Value),
    Join(Vec<RowCol>),
    Emit([RowCol; 3]),
}

pub fn constrain(constraints: &[Constraint], indexes: &[[Vec<Value>; 3]], ranges: &mut [LoHi], results: &mut [Vec<Value>; 3]) -> () {
    if constraints.len() > 0 {
        match &constraints[0] {
            &Constraint::Constant((row_ix, col_ix), ref value) => {
                let column = &indexes[row_ix][col_ix];
                let (old_lo, old_hi) = ranges[row_ix];
                let lo = gallop(column, old_lo, old_hi, |v| v < value);
                let hi = gallop(column, lo, old_hi, |v| v <= value);
                if lo < hi {
                    ranges[row_ix] = (lo, hi);
                    constrain(&constraints[1..], indexes, ranges, results);
                    ranges[row_ix] = (old_lo, old_hi);
                }
            }
            &Constraint::Join(ref rowcols) => {
                let mut buffer = vec![(0,0); rowcols.len()]; // TODO pre-allocate
                let (row_ix, col_ix) = rowcols[0]; // TODO pick smallest
                let column = &indexes[row_ix][col_ix];
                let (old_lo, old_hi) = ranges[row_ix];
                let mut lo = old_lo;
                // loop over rowcols[0]
                while lo < old_hi {
                    let value = &column[lo];
                    let hi = gallop(column, lo + 1, old_hi, |v| v <= value);
                    ranges[row_ix] = (lo, hi);
                    {
                        // loop over rowcols[1..]
                        let mut i = 1;
                        while i < rowcols.len() {
                            let column = &indexes[row_ix][col_ix];
                            let (old_lo, old_hi) = ranges[row_ix];
                            let lo = gallop(column, old_lo, old_hi, |v| v < value);
                            let hi = gallop(column, lo, old_hi, |v| v <= value);
                            if lo < hi {
                                ranges[row_ix] = (lo, hi);
                                buffer[i] = (old_lo, old_hi);
                                i += 1;
                            } else {
                                break;
                            }
                        }
                        // if all succeeded, continue with rest of constraints
                        if i == rowcols.len() {
                            constrain(&constraints[1..], indexes, ranges, results);
                        }
                        // restore state for rowcols[1..i]
                        while i > 1 {
                            i -= 1;
                            let (row_ix, _) = rowcols[i];
                            ranges[row_ix] = buffer[i];
                        }
                    }
                    lo = hi;
                }
                // restore state for rowcols[0]
                ranges[row_ix] = (old_lo, old_hi);
            }
            &Constraint::Emit(row_and_col_ixes) => {
                for (result, &(row_ix, col_ix)) in results.iter_mut().zip(row_and_col_ixes.iter()) {
                    let (lo, _) = ranges[row_ix];
                    result.push(indexes[row_ix][col_ix][lo].clone());
                }
                constrain(&constraints[1..], indexes, ranges, results);
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
    fn solve(&self, bag: &Bag) -> [Vec<Value>; 3] {
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
        let indexes: Vec<[Vec<Value>; 3]> = self.row_orderings
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
                [
                    ordered_eavs.iter().map(|eav| eav[0].clone()).collect(),
                    ordered_eavs.iter().map(|eav| eav[1].clone()).collect(),
                    ordered_eavs.iter().map(|eav| eav[2].clone()).collect(),
                ]
            })
            .collect();
        let mut ranges: Vec<LoHi> = indexes.iter().map(|index| (0, index[0].len())).collect();
        let mut results = [vec![], vec![], vec![]];
        constrain(&*self.constraints, &*indexes, &mut *ranges, &mut results);
        results
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueExpr {
    Constant(Value),
    Variable(String),
}

#[derive(Debug, Clone)]
pub enum RowExpr {
    Pattern([ValueExpr; 3]),
    Assert([ValueExpr; 3]),
}

#[derive(Debug, Clone)]
pub struct QueryExpr {
    rows:Vec<RowExpr>,
}

fn compile(query_expr: &QueryExpr) -> Result<Query, String> {

    // sort variables
    let mut constants: Vec<(&Value, (usize, usize))> = vec![];
    let mut variables: Vec<(&String, Vec<(usize, usize)>)> = vec![];
    for (row, row_expr) in query_expr.rows.iter().enumerate() {
        match row_expr {
            &RowExpr::Pattern(ref value_exprs) => {
                for (col, value_expr) in value_exprs.iter().enumerate() {
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
            _ => ()
        }
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
    let row_orderings:Vec<[usize; 3]> = query_expr
        .rows
        .iter()
        .filter(|r| match r { &&RowExpr::Pattern(_) => true, _ => false})
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

    // remap columns
    let mut cols_after_sort: HashMap<(usize, usize), usize> = HashMap::new();
    for (row, row_ordering) in row_orderings.iter().enumerate() {
        for col in 0..3 {
            let col_after_sort = row_ordering.iter().position(|c| *c == col).unwrap();
            cols_after_sort.insert((row, col), col_after_sort);
        }
    }

    println!("{:?}", cols_after_sort);

    // create constants
    let mut constraints = vec![];
    for &(value, (row, col)) in constants.iter() {
        println!("{:?}", (row, col));
        constraints.push(Constraint::Constant((row, *cols_after_sort.get(&(row, col)).unwrap()), value.clone()));
    }

    // create joins
    for &(_, ref rows_and_cols) in variables.iter() {
        constraints.push(Constraint::Join(
            rows_and_cols.iter().map(|&(row, col)| (row, *cols_after_sort.get(&(row, col)).unwrap())).collect(),
        ));
    }

    // create emits
    for (row, row_expr) in query_expr.rows.iter().enumerate() {
        match row_expr {
            &RowExpr::Assert(ref value_exprs) => {
                let mut row_and_col_ixes = [(0, 0), (0, 0), (0, 0)];
                for (col, value_expr) in value_exprs.iter().enumerate() {
                    match value_expr {
                        &ValueExpr::Constant(_) => return Err(format!("Can't assert constants yet (row {} col {})", row, col)),
                        &ValueExpr::Variable(ref variable) => {
                            match variables.iter().position(|&(ref v, _)| *v == variable) {
                                None => return Err(format!("Variable {:?} is not bound (row {} col {})", variable, row, col)),
                                Some(ix) => {
                                    let (r,c) = variables[ix].1[0];
                                    row_and_col_ixes[col] = (r, *cols_after_sort.get(&(r,c)).unwrap());
                                }
                            }
                        }
                    }
                }
                constraints.push(Constraint::Emit(row_and_col_ixes));
            }
            _ => ()
        }
    }


    Ok(Query {
        row_orderings,
        constraints,
    })
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

    named!(values_expr(&[u8]) -> [ValueExpr;3], dbg_dmp!(do_parse!(
    v1: value_expr >>
    space >>
    v2: value_expr >>
    space >>
    v3: value_expr >>
    ([v1,v2,v3])
    )));

    named!(row_expr(&[u8]) -> RowExpr, alt!(
        do_parse!(
            tag!("+") >>
            space >>
            values: values_expr >>
                (RowExpr::Assert(values)))
        |
        do_parse!(
            values: values_expr >>
                (RowExpr::Pattern(values)))
        
    ));

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
            Err(error) => format!("{}\n\n{:?}\n\n{:?}", codelet, cursor, error),
            Ok(query_expr) => {
                match compile(&query_expr) {
                    Err(error) => format!("{}\n\n{:?}\n\n{:?}\n\n{}", codelet, cursor, query_expr, error),
                    Ok(query) => format!("{}\n\n{:?}\n\n{:?}\n\n{:?}\n\n{:?}", codelet, cursor, query_expr, query, query.solve(&bag))
                }
                
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

// #[derive(Debug, Serialize, Deserialize)]
// pub enum Command {
//     Render(String),
// }

// fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
//     sender
//         .send_message(&OwnedMessage::Text(json!(c).to_string()))
//         .unwrap()
// }

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
