#![feature(proc_macro)]
#![feature(conservative_impl_trait)]
#![feature(slice_patterns)]

extern crate websocket;
#[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate nom;
extern crate csv;

extern crate timely;
extern crate differential_dataflow;
extern crate graph_map;
#[macro_use]
extern crate abomonation;

use std::thread;
use std::sync::{Arc, Mutex};

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::fs::File;
use std::io::prelude::*;

use std::collections::{HashMap, BTreeMap};
use std::iter::Iterator;

use std::borrow::Cow;
use std::borrow::Borrow;

use nom::*;

use std::error::Error;

use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

use timely::dataflow::*;
use timely::dataflow::operators::*;

use differential_dataflow::Collection;
use differential_dataflow::lattice::Lattice;
use differential_dataflow::operators::*;

use differential_dataflow::operators::arrange::ArrangeBySelf;
use differential_dataflow::operators::arrange::ArrangeByKey;

use graph_map::GraphMMap;

use abomonation::Abomonation;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
struct Entity {
    avs: Vec<(Attribute, Value)>,
}

type Attribute = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
enum Value {
    Boolean(bool),
    Integer(i64),
    String(String),
    Entity(u64),
}

unsafe_abomonate!(Entity: avs);

impl Abomonation for Value {
    #[inline]
    unsafe fn embalm(&mut self) {
        match self {
            &mut Value::Boolean(ref mut inner) => inner.embalm(),
            &mut Value::Integer(ref mut inner) => inner.embalm(),
            &mut Value::String(ref mut inner) => inner.embalm(),
            &mut Value::Entity(ref mut inner) => inner.embalm(),
        }
    }

    #[inline]
    unsafe fn entomb(&self, bytes: &mut Vec<u8>) {
        match self {
            &Value::Boolean(ref inner) => inner.entomb(bytes),
            &Value::Integer(ref inner) => inner.entomb(bytes),
            &Value::String(ref inner) => inner.entomb(bytes),
            &Value::Entity(ref inner) => inner.entomb(bytes),
        }
    }

    #[inline]
    unsafe fn exhume<'a, 'b>(&'a mut self, bytes: &'b mut [u8]) -> Option<&'b mut [u8]> {
        match self {
            &mut Value::Boolean(ref mut inner) => inner.exhume(bytes),
            &mut Value::Integer(ref mut inner) => inner.exhume(bytes),
            &mut Value::String(ref mut inner) => inner.exhume(bytes),
            &mut Value::Entity(ref mut inner) => inner.exhume(bytes),
        }
    }
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
            &Value::String(ref string) => write!(f, "{:?}", string),
            &Value::Entity(ref entity) => write!(f, "#{}", entity),
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

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
struct Bag {
    entities: HashMap<u64, Entity>,
    eavs: HashMap<(u64, Attribute), Value>,
}

impl Bag {
    fn new() -> Self {
        Bag {
            entities: HashMap::new(),
            eavs: HashMap::new(),
        }
    }

    fn create(&mut self, avs: Vec<(String, Value)>) -> Value {
        let entity = Entity { avs: avs.clone() };
        let mut hasher = DefaultHasher::new();
        entity.hash(&mut hasher);
        let hash = hasher.finish();
        self.entities.insert(hash, entity);
        for (attribute, value) in avs {
            self.eavs.insert((hash, attribute), value);
        }
        Value::Entity(hash)
    }

    // this takes tuple instead of array because it was triggering some compiler bug
    fn insert(&mut self, eav: (Value, Value, Value)) {
        match eav {
            (Value::Entity(e), Value::String(a), v) => self.eavs.insert((e, a), v),
            other => panic!("Weird eav insert: {:?}", other),
        };
    }
}

fn parse_chinook(field: &str) -> Value {
    match field.parse::<i64>() {
        Ok(i) => Value::Integer(i),
        Err(_) => Value::String(field.to_owned()),
    }
}

fn chinook() -> Result<Bag, Box<Error>> {
    let mut bag = Bag::new();
    for filename in vec![
        "Album.csv",
        "Customer.csv",
        "Genre.csv",
        "InvoiceLine.csv",
        "MediaType.csv",
        "PlaylistTrack.csv",
        "Artist.csv",
        "Employee.csv",
        "Invoice.csv",
        "Playlist.csv",
        "Track.csv",
    ]
    {
        let mut reader = csv::ReaderBuilder::new().delimiter(b'\t').from_reader(
            File::open(
                format!(
                    "./data/{}",
                    filename
                ),
            )?,
        );
        let headers = reader.headers()?.clone();
        for record_or_error in reader.records() {
            let record = record_or_error?;
            let avs = headers
                .iter()
                .map(|s| s.to_lowercase())
                .zip(record.iter().map(parse_chinook))
                .collect();
            bag.create(avs);
        }
    }
    Ok(bag)
}

// fn load() -> Bag {
//     let mut file = File::open("/home/jamie/imp.db").unwrap();
//     let mut contents = String::new();
//     file.read_to_string(&mut contents).unwrap();
//     let eavs: Vec<((Entity, Attribute), Value)> = serde_json::from_str(&*contents).unwrap();
//     Bag { eavs: eavs.into_iter().collect() }
// }

fn save(bag: &Bag) {
    let mut file = File::create("/home/jamie/imp.db").unwrap();
    write!(
        file,
        "{}",
        json!(bag.eavs.clone().into_iter().collect::<Vec<_>>())
    ).unwrap();
}

// f should be |t| t < value or |t| t <= value
fn gallop<'a, T, F: Fn(&T) -> bool>(slice: &'a [T], mut lo: usize, hi: usize, f: F) -> usize {
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

#[derive(Debug, Clone)]
enum Function {
    Add(usize, usize),
}

impl Function {
    fn apply(&self, variables: &[Value]) -> Result<Value, String> {
        match self {
            &Function::Add(a, b) => {
                match (&variables[a], &variables[b]) {
                    (&Value::Integer(a), &Value::Integer(b)) => Ok(Value::Integer(a + b)),
                    (a, b) => Err(format!("Type error: {} + {}", a, b)),
                }
            }
        }
    }
}

type LoHi = (usize, usize);
type RowCol = (usize, usize);

#[derive(Debug, Clone)]
enum Constraint {
    Join(usize, bool, Vec<RowCol>),
    Apply(usize, bool, Function),
    Assert([usize; 3]),
    Debug(Vec<(String, usize)>),
}

#[derive(Debug, Clone)]
struct Block {
    row_orderings: Vec<[usize; 3]>,
    variables: Vec<Value>,
    constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprIr {
    Constant(Value),
    Variable(String),
    Function(FunctionIr),
    Dot(usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionIr {
    name: String,
    args: Vec<usize>,
}

impl ExprIr {
    fn is_constant(&self) -> bool {
        match self {
            &ExprIr::Constant(_) => true,
            _ => false,
        }
    }

    fn is_variable(&self) -> bool {
        match self {
            &ExprIr::Variable(_) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            &ExprIr::Function(_) => true,
            _ => false,
        }
    }
}

fn translate(expr: &ExprAst, exprs: &mut Vec<ExprIr>) -> usize {
    let expr = match expr {
        &ExprAst::Constant(ref value) => ExprIr::Constant(value.clone()),
        &ExprAst::Variable(ref variable) => ExprIr::Variable(variable.clone()),
        &ExprAst::Function(FunctionAst { ref name, ref args }) => ExprIr::Function(FunctionIr {
            name: name.clone(),
            args: args.iter().map(|e| translate(e, exprs)).collect(),
        }),
        &ExprAst::Dot(ref expr1, ref expr2) => {
            ExprIr::Dot(translate(expr1, exprs), translate(expr2, exprs))
        }
    };
    exprs.push(expr);
    exprs.len() - 1
}

fn compile(block: &BlockAst) -> Result<Block, String> {

    // label exprs in pre-order and flatten tree
    let mut expr_irs: Vec<ExprIr> = vec![];
    let mut pattern_exprs: Vec<[usize; 2]> = vec![];
    let mut assert_exprs: Vec<[usize; 3]> = vec![];
    for statement_or_error in block.statements.iter() {
        match statement_or_error {
            &Ok(ref statement) => {
                match statement {
                    &StatementAst::Pattern([ref e1, ref e2]) => {
                        pattern_exprs.push(
                            [
                                translate(e1, &mut expr_irs),
                                translate(e2, &mut expr_irs),
                            ],
                        );
                    }
                    &StatementAst::Assert([ref e, ref a, ref v]) => {
                        assert_exprs.push(
                            [
                                translate(e, &mut expr_irs),
                                translate(a, &mut expr_irs),
                                translate(v, &mut expr_irs),
                            ],
                        );
                    }
                }
            }
            &Err(_) => (),
        }
    }

    // group exprs that must be equal
    let mut expr_group: Vec<usize> = (0..expr_irs.len()).collect();

    // all variables with same name must be equal
    let mut variable_group: HashMap<&str, usize> = HashMap::new();
    for (expr, ir) in expr_irs.iter().enumerate() {
        match ir {
            &ExprIr::Variable(ref variable) => {
                match variable_group.get(&**variable) {
                    Some(&group) => expr_group[expr] = group,
                    None => {
                        variable_group.insert(variable, expr);
                    }
                }
            }
            _ => (),
        }
    }

    // both exprs in a top-level pattern (eg a = 2) must be equal
    for &[expr1, expr2] in pattern_exprs.iter() {
        let group1 = expr_group[expr1];
        let group2 = expr_group[expr2];
        for group in expr_group.iter_mut() {
            if *group == group2 {
                *group = group1;
            }
        }
    }

    // gather up groups
    let mut group_exprs: HashMap<usize, Vec<usize>> = HashMap::new();
    for (expr, group) in expr_group.iter().enumerate() {
        group_exprs.entry(*group).or_insert_with(|| vec![]).push(
            expr,
        );
    }

    // sort by order of appearance in code
    let mut slot_exprs: Vec<Vec<usize>> = group_exprs
        .iter()
        .map(|(_, exprs)| {
            let mut exprs = exprs.clone();
            exprs.sort_unstable();
            exprs
        })
        .collect();
    slot_exprs.sort_unstable_by_key(|exprs| exprs[0]);

    // move slots that contain constants and no functions to the start
    for slot in 0..slot_exprs.len() {
        if slot_exprs[slot].iter().any(
            |&expr| expr_irs[expr].is_constant(),
        ) &&
            slot_exprs[slot].iter().all(
                |&expr| !expr_irs[expr].is_function(),
            )
        {
            let exprs = slot_exprs.remove(slot);
            slot_exprs.insert(0, exprs);
        }
    }

    // index in the other direction
    let mut expr_slot: Vec<usize> = (0..expr_irs.len()).collect();
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        for expr in exprs.iter() {
            expr_slot[*expr] = slot;
        }
    }

    // collect exprs that directly query the database
    let mut row_exprs: Vec<[usize; 3]> = vec![];
    for (v, ir) in expr_irs.iter().enumerate() {
        match ir {
            &ExprIr::Dot(e, a) => row_exprs.push([e, a, v]),
            _ => (),
        }
    }

    // choose row indexes with columns in the order they appear in the slots
    let row_orderings: Vec<[usize; 3]> = row_exprs
        .iter()
        .map(|exprs| {
            let mut ordering = [0, 1, 2];
            ordering.sort_unstable_by_key(|&ix| expr_slot[exprs[ix]]);
            ordering
        })
        .collect();

    // produce constraints
    let mut values: Vec<Value> = (0..slot_exprs.len())
        .map(|_| Value::Boolean(false))
        .collect();
    let mut constraints: Vec<Constraint> = vec![];
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        // gather up everything that constrains this slot
        let constants: Vec<&ExprIr> = exprs
            .iter()
            .map(|&expr| &expr_irs[expr])
            .filter(|ir| ir.is_constant())
            .collect();
        let functions: Vec<&ExprIr> = exprs
            .iter()
            .map(|&expr| &expr_irs[expr])
            .filter(|ir| ir.is_function())
            .collect();
        let mut rowcols: Vec<(usize, usize)> = vec![];
        for expr in exprs.iter() {
            for (row, cols) in row_exprs.iter().enumerate() {
                for (col, rc_expr) in cols.iter().enumerate() {
                    if expr == rc_expr {
                        rowcols.push((row, col));
                    }
                }
            }
        }

        // check constants are sane
        for i in 1..constants.len() {
            if constants[i] != constants[0] {
                return Err(format!(
                    "Impossible constraint: {:?} = {:?}",
                    constants[i],
                    constants[0]
                ));
            }
        }

        // check that something constrains this slot
        if (constants.len() == 0) && (functions.len() == 0) && (rowcols.len() == 0) {
            return Err(format!("No constraints on slot {}", slot)); // TODO how to identify?
        }

        // after first constant or function, the rest just have to check their result is equal
        let mut slot_fixed_yet = false;

        // constants just get stuck in the values vec
        if constants.len() > 0 {
            values[slot] = match constants[0] {
                &ExprIr::Constant(ref value) => value.clone(),
                _ => unreachable!(),
            };
            slot_fixed_yet = true;
        }

        // functions get run next
        for function in functions.iter() {
            match function {
                &&ExprIr::Function(FunctionIr { ref name, ref args }) => {
                    let function = match (&**name, &**args) {
                        ("+", &[a, b]) => Function::Add(expr_slot[a], expr_slot[b]),
                        _ => {
                            return Err(format!(
                                "I don't know any function called {:?} with {} arguments",
                                name,
                                args.len()
                            ))
                        }
                    };
                    constraints.push(Constraint::Apply(slot, slot_fixed_yet, function));
                    slot_fixed_yet = true;
                }
                _ => unreachable!(),
            }
        }

        // and then joins
        constraints.push(Constraint::Join(slot, slot_fixed_yet, rowcols));
    }

    // asserts are constraints too
    // TODO constraints is the wrong name for anything that includes asserts
    for exprs in assert_exprs.iter() {
        constraints.push(Constraint::Assert(
            [
                expr_slot[exprs[0]],
                expr_slot[exprs[1]],
                expr_slot[exprs[2]],
            ],
        ));
    }

    let mut named_variables: Vec<(String, usize)> = vec![];
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        if let Some(&ExprIr::Variable(ref name)) =
            exprs
                .iter()
                .map(|&expr| &expr_irs[expr])
                .filter(|ir| ir.is_variable())
                .next()
        {
            named_variables.push((name.clone(), slot));
        }
    }
    constraints.push(Constraint::Debug(named_variables));

    // println!(
    //     "{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}",
    //     expr_irs,
    //     pattern_exprs,
    //     assert_exprs,
    //     expr_group,
    //     group_exprs,
    //     slot_exprs,
    //     expr_slot,
    //     row_exprs,
    //     row_orderings,
    //     values,
    //     constraints,
    // );

    Ok(Block {
        row_orderings,
        variables: values,
        constraints,
    })
}

#[derive(Debug, Clone)]
struct CodeAst {
    blocks: Vec<BlockAst>,
    focused: Option<usize>,
}

#[derive(Debug, Clone)]
struct BlockAst {
    statements: Vec<Result<StatementAst, String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprAst {
    Constant(Value),
    Variable(String),
    Function(FunctionAst),
    Dot(Box<ExprAst>, Box<ExprAst>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionAst {
    name: String,
    args: Vec<ExprAst>,
}

#[derive(Debug, Clone)]
enum StatementAst {
    Pattern([ExprAst; 2]),
    Assert([ExprAst; 3]),
}

fn simplify_errors<Output>(result: IResult<&[u8], Output>) -> Result<Output, String> {
    match result {
        IResult::Done(remaining, output) => {
            if remaining.len() <= 1 {
                // because of the hacky \n on the end
                Ok(output)
            } else {
                Err(format!(
                    "Remaining: {}",
                    std::str::from_utf8(remaining).unwrap()
                ))
            }
        }
        IResult::Error(error) => Err(format!("Nom error: {:?}", error)),
        IResult::Incomplete(needed) => Err(format!("Nom incomplete: {:?}", needed)),
    }
}

fn code_ast(text: &str, cursor: i64) -> CodeAst {
    let blocks = text.split("\n\n")
        .map(|block| block_ast(block))
        .collect::<Vec<_>>();
    let mut focused = None;
    let mut remaining_cursor = cursor;
    for (i, block_src) in text.split("\n\n").enumerate() {
        remaining_cursor -= block_src.len() as i64;
        if remaining_cursor <= 0 {
            focused = Some(i);
            break;
        }
        remaining_cursor -= 2; // \n\n
        if remaining_cursor < 0 {
            break;
        }
    }
    CodeAst { blocks, focused }
}

fn block_ast(text: &str) -> BlockAst {
    BlockAst {
        statements: text.split("\n")
            .map(|statement| {
                let statement = format!("{}\n", statement); // hack to get nom to stop streaming
                simplify_errors(statement_ast(statement.as_bytes()))
            })
            .collect::<Vec<_>>(),
    }
}

named!(statement_ast(&[u8]) -> StatementAst, alt!(assert_ast | pattern_ast));

named!(assert_ast(&[u8]) -> StatementAst, do_parse!(
    tag!("+") >>
    space >>
    e: simple_expr_ast >>
    tag!(".") >>
    a: symbol_ast >>
    opt!(space) >>
    tag!("=") >>
    opt!(space) >>
    v: expr_ast >>
    (StatementAst::Assert([e, ExprAst::Constant(Value::String(a)), v]))
));

named!(pattern_ast(&[u8]) -> StatementAst, do_parse!(
    e1: expr_ast >>
    opt!(space) >>
    tag!("=") >>
    opt!(space) >>
    e2: expr_ast >>
    (StatementAst::Pattern([e1, e2]))
));

named!(expr_ast(&[u8]) -> ExprAst, do_parse!(
    e: simple_expr_ast >>
        ts: many0!(dot_ast) >>
        f: opt!(infix_function_ast) >>
        ({
            let mut e = e;
            for t in ts {
                e = ExprAst::Dot(Box::new(e), Box::new(ExprAst::Constant(Value::String(t))));
            }
            if let Some((name, arg)) = f {
                e = ExprAst::Function(FunctionAst{name, args: vec![e, arg]});
            }
            e
        })
));

named!(simple_expr_ast(&[u8]) -> ExprAst, alt!(
        map!(prefix_function_ast, ExprAst::Function) |
        map!(value_ast, ExprAst::Constant) |
        map!(symbol_ast, ExprAst::Variable) |
        paren_ast
));

named!(dot_ast(&[u8]) -> String, do_parse!(
    tag!(".") >>
    a: symbol_ast >>
        (a)
        ));

named!(prefix_function_ast(&[u8]) -> FunctionAst, do_parse!(
        name: symbol_ast >>
        tag!("(") >>
        args: separated_list_complete!(tuple!(tag!(","), opt!(space)), expr_ast) >>
        tag!(")") >>
        (FunctionAst{name, args})
    ));

named!(infix_function_ast(&[u8]) -> (String, ExprAst), do_parse!(
        opt!(space) >>
        name: map_res!(
            alt!(tag!("+")),
            |b| std::str::from_utf8(b).map(|s| s.to_owned())
        ) >>
        opt!(space) >>
        arg: expr_ast >>
        (name, arg)
));

named!(symbol_ast(&[u8]) -> String, map_res!(
    verify!(
        take_while1_s!(|c| is_alphanumeric(c) || c == ('-' as u8)),
        |b: &[u8]| is_alphabetic(b[0])
    ),
    |b| std::str::from_utf8(b).map(|s| s.to_owned())
));

named!(value_ast(&[u8]) -> Value, alt!(
    map!(integer_ast, Value::Integer) |
    map!(boolean_ast, Value::Boolean) |
    map!(string_ast, Value::String)
));

named!(integer_ast(&[u8]) -> i64, map_res!(
        digit,
        |b| std::str::from_utf8(b).unwrap().parse::<i64>()
    ));

named!(boolean_ast(&[u8]) -> bool, do_parse!(
        b: alt!(tag!("true") | tag!("false")) >>
        (b == b"true")
    ));

// TODO escaping
named!(string_ast(&[u8]) -> String, map_res!(
        delimited!(char!('"'), take_until!("\""), char!('"')),
        |b| std::str::from_utf8(b).map(|s| s.to_owned())
    ));

named!(paren_ast(&[u8]) -> ExprAst, do_parse!(
        tag!("(") >>
        opt!(space) >>
        e: expr_ast >>
        opt!(space) >>
        tag!(")") >>
        (e)
    ));

#[derive(Debug, Serialize, Deserialize)]
enum EditorEvent {
    State(String, i64),
}

// #[derive(Debug, Serialize, Deserialize)]
// enum Command {
//     Render(String),
// }

// fn send_command(sender: &mut websocket::sender::Writer<std::net::TcpStream>, c: Command) {
//     sender
//         .send_message(&OwnedMessage::Text(json!(c).to_string()))
//         .unwrap()
// }

fn serve_editor() {
    let state = Arc::new(Mutex::new(("".to_owned(), 0)));

    let server = Server::bind("127.0.0.1:8081").unwrap();

    thread::spawn({
        let state = state.clone();
        move || {
            let bag = chinook().unwrap();
            println!("Bag is {:?}", bag);
            let mut last_state = state.lock().unwrap().clone();
            loop {
                let state = &*state.lock().unwrap();
                if *state != last_state {
                    print!("\x1b[2J\x1b[1;1H");
                    let &(ref code, cursor) = state;
                    let start = ::std::time::Instant::now();
                    // run_code(&mut bag.clone(), &*code, cursor);
                    let elapsed = start.elapsed();
                    println!(
                        "In {} ms",
                        (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64
                    );
                    last_state = state.clone();
                }
            }
        }
    });

    for request in server.filter_map(Result::ok) {
        let state = state.clone();
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
                        // println!("Received: {}", text);
                        let event: EditorEvent = serde_json::from_str(text).unwrap();
                        match event {
                            EditorEvent::State(code, cursor) => {
                                *state.lock().unwrap() = (code, cursor);
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

fn get_all(row: &[Value], key: &[usize]) -> Vec<Value> {
    key.iter().map(|&ix| row[ix].clone()).collect()
}

fn serve_dataflow() {
    let code = std::env::args().skip(1).next().unwrap();
    println!("Running:\n{}", code);
    timely::execute_from_args(std::env::args().skip(1), move |worker| {

        let peers = worker.peers();
        let index = worker.index();
        println!("peers {:?} index {:?}", peers, index);

        let eavs = chinook()
            .unwrap()
            .eavs
            .into_iter()
            .map(|((e, a), v)| {
                (
                    vec![Value::Entity(e), Value::String(a), v],
                    Default::default(),
                    1,
                )
            })
            .collect::<Vec<_>>();

        let code = code.clone();
        worker.dataflow::<(), _, _>(move |scope| {
            let eavs: Collection<_, Vec<Value>, _> = Collection::new(eavs.to_stream(scope));

            let block = compile(&block_ast(&*code)).unwrap();
            println!("{:?}", block);

            let mut rc_var: HashMap<RowCol, usize> = HashMap::new();

            let mut variables: Collection<_, Vec<Value>, _> =
                Collection::new(
                    vec![(block.variables.clone(), Default::default(), 1)].to_stream(scope),
                );
            for constraint in block.constraints.iter() {
                match constraint {
                    &Constraint::Join(var, result_already_fixed, ref rcs) => {
                        let mut result_already_fixed = result_already_fixed;
                        for &(r, c) in rcs.iter() {
                            let r = r.clone();
                            let c = c.clone();
                            let mut variables_key = vec![];
                            let mut eav_key = vec![];
                            if result_already_fixed {
                                variables_key.push(var);
                                eav_key.push(c);
                            }
                            for c2 in 0..3 {
                                if let Some(&var2) = rc_var.get(&(r, c2)) {
                                    variables_key.push(var2);
                                    eav_key.push(c2);
                                }
                            }
                            let index = eavs.map(move |row| (get_all(&*row, &*eav_key), row))
                                .arrange_by_key();
                            variables = variables
                                .map(move |row| (get_all(&*row, &*variables_key), row))
                                .arrange_by_key()
                                .join_core(&index, move |_key, row, eav| {
                                    let mut row = row.clone();
                                    row[var] = eav[c].clone();
                                    vec![row]
                                });
                            result_already_fixed = true;
                            rc_var.insert((r, c), var);
                        }
                    }
                    &Constraint::Apply(var, result_already_fixed, ref function) => {
                        let var = var.clone();
                        let function = function.clone();
                        if result_already_fixed {
                            variables = variables.filter(move |row| {
                                let result = function.apply(&*row).unwrap();
                                row[var] == result
                            });
                        } else {
                            variables = variables.map(move |mut row| {
                                let result = function.apply(&*row).unwrap();
                                row[var] = result;
                                row
                            });
                        }
                    }
                    &Constraint::Assert(ref vars) => {}
                    &Constraint::Debug(ref names_and_vars) => {
                        let names_and_vars = names_and_vars.clone();
                        variables.inspect(move |&(ref row, _, _)| {
                            let mut output = String::new();
                            for &(ref name, var) in names_and_vars.iter() {
                                output.push_str(&*format!("{}={:?}\t", name, row[var]));
                            }
                            println!("{}", output);
                        });
                    }
                }
            }
        });

    }).unwrap();
}

fn main() {
    // serve_editor();
    serve_dataflow();
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
