use std::fs::File;

use std::collections::HashMap;
use std::iter::Iterator;
use std::borrow::Borrow;

use nom::*;

use std::error::Error;

use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub struct Entity {
    pub avs: Vec<(Attribute, Value)>,
}

pub type Attribute = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    String(String),
    Entity(u64),
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

impl ::std::fmt::Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &Value::Boolean(bool) => bool.fmt(f),
            &Value::Integer(integer) => integer.fmt(f),
            &Value::String(ref string) => write!(f, "{:?}", string),
            &Value::Entity(ref entity) => write!(f, "#{}", entity),
        }
    }
}

impl Value {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            &Value::String(ref this) => Some(this),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            &Value::Integer(this) => Some(this),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
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
pub struct Bag {
    pub entities: HashMap<u64, Entity>,
    pub eavs: HashMap<(u64, Attribute), Value>,
}

impl Bag {
    pub fn new() -> Self {
        Bag {
            entities: HashMap::new(),
            eavs: HashMap::new(),
        }
    }

    pub fn create(&mut self, avs: Vec<(String, Value)>) -> Value {
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
    pub fn insert(&mut self, eav: (Value, Value, Value)) {
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

pub fn chinook() -> Result<Bag, Box<Error>> {
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
        let mut reader = ::csv::ReaderBuilder::new().delimiter(b'\t').from_reader(
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

#[derive(Debug, Clone)]
pub enum Function {
    Add(usize, usize),
}

impl Function {
    pub fn apply<V>(&self, variables: &[V]) -> Result<Value, String>
    where
        V: Borrow<Value>,
    {
        match self {
            &Function::Add(a, b) => {
                match (variables[a].borrow(), variables[b].borrow()) {
                    (&Value::Integer(a), &Value::Integer(b)) => Ok(Value::Integer(a + b)),
                    (a, b) => Err(format!("Type error: {} + {}", a, b)),
                }
            }
        }
    }
}

pub type RowCol = (usize, usize);

#[derive(Debug, Clone)]
pub enum Constraint {
    Join(usize, bool, Vec<RowCol>),
    Apply(usize, bool, Function),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub row_orderings: Vec<Vec<usize>>,
    pub variables: Vec<Value>,
    pub constraints: Vec<Constraint>,
    pub result_vars: Vec<(String, usize)>,
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

fn translate(expr: &ExprAst, output_expr_irs: &mut Vec<ExprIr>) -> usize {
    let expr = match expr {
        &ExprAst::Constant(ref value) => ExprIr::Constant(value.clone()),
        &ExprAst::Variable(ref variable) => ExprIr::Variable(variable.clone()),
        &ExprAst::Function(FunctionAst { ref name, ref args }) => ExprIr::Function(FunctionIr {
            name: name.clone(),
            args: args.iter().map(|e| translate(e, output_expr_irs)).collect(),
        }),
        &ExprAst::Dot(ref expr1, ref expr2) => {
            ExprIr::Dot(
                translate(expr1, output_expr_irs),
                translate(expr2, output_expr_irs),
            )
        }
    };
    output_expr_irs.push(expr);
    output_expr_irs.len() - 1
}

pub fn plan(block: &BlockAst) -> Result<Block, String> {

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
    let row_orderings: Vec<Vec<usize>> = row_exprs
        .iter()
        .map(|exprs| {
            let mut ordering = vec![0, 1, 2];
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

    // for now, just output any named variable
    let mut result_vars: Vec<(String, usize)> = vec![];
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        if let Some(&ExprIr::Variable(ref name)) =
            exprs
                .iter()
                .map(|&expr| &expr_irs[expr])
                .filter(|ir| ir.is_variable())
                .next()
        {
            result_vars.push((name.clone(), slot));
        }
    }

    Ok(Block {
        row_orderings,
        variables: values,
        constraints,
        result_vars,
    })
}

#[derive(Debug, Clone)]
pub struct CodeAst {
    pub blocks: Vec<BlockAst>,
    pub focused: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct BlockAst {
    pub statements: Vec<Result<StatementAst, String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprAst {
    Constant(Value),
    Variable(String),
    Function(FunctionAst),
    Dot(Box<ExprAst>, Box<ExprAst>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionAst {
    name: String,
    args: Vec<ExprAst>,
}

#[derive(Debug, Clone)]
pub enum StatementAst {
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
                    ::std::str::from_utf8(remaining).unwrap()
                ))
            }
        }
        IResult::Error(error) => Err(format!("Nom error: {:?}", error)),
        IResult::Incomplete(needed) => Err(format!("Nom incomplete: {:?}", needed)),
    }
}

pub fn code_ast(text: &str, cursor: i64) -> CodeAst {
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

pub fn block_ast(text: &str) -> BlockAst {
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
            |b| ::std::str::from_utf8(b).map(|s| s.to_owned())
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
    |b| ::std::str::from_utf8(b).map(|s| s.to_owned())
));

named!(value_ast(&[u8]) -> Value, alt!(
    map!(integer_ast, Value::Integer) |
    map!(boolean_ast, Value::Boolean) |
    map!(string_ast, Value::String)
));

named!(integer_ast(&[u8]) -> i64, map_res!(
        digit,
        |b| ::std::str::from_utf8(b).unwrap().parse::<i64>()
    ));

named!(boolean_ast(&[u8]) -> bool, do_parse!(
        b: alt!(tag!("true") | tag!("false")) >>
        (b == b"true")
    ));

// TODO escaping
named!(string_ast(&[u8]) -> String, map_res!(
        delimited!(char!('"'), take_until!("\""), char!('"')),
        |b| ::std::str::from_utf8(b).map(|s| s.to_owned())
    ));

named!(paren_ast(&[u8]) -> ExprAst, do_parse!(
        tag!("(") >>
        opt!(space) >>
        e: expr_ast >>
        opt!(space) >>
        tag!(")") >>
        (e)
    ));
