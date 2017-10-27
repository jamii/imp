use std::fs::File;

use std::collections::{HashMap, HashSet};
use std::iter::Iterator;
use std::borrow::{Cow, Borrow};

use nom::*;

use std::error::Error;

use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub struct Entity {
    pub avs: Vec<(Attribute, Value<'static>)>,
}

pub type Attribute = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Clone)]
pub enum Value<'a> {
    Boolean(bool),
    Integer(i64),
    String(Cow<'a, str>),
    Entity(u64),
}

impl<'a> From<bool> for Value<'a> {
    fn from(bool: bool) -> Value<'a> {
        Value::Boolean(bool)
    }
}

impl<'a> From<i64> for Value<'a> {
    fn from(integer: i64) -> Value<'a> {
        Value::Integer(integer)
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(string: String) -> Value<'a> {
        Value::String(Cow::Owned(string))
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(string: &'a str) -> Value<'a> {
        Value::String(Cow::Borrowed(string))
    }
}

impl<'a> ::std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &Value::Boolean(bool) => bool.fmt(f),
            &Value::Integer(integer) => integer.fmt(f),
            &Value::String(ref string) => write!(f, "{:?}", string),
            &Value::Entity(entity) => write!(f, "#{}", entity),
        }
    }
}

impl<'a> Value<'a> {
    // for various reasons, we can't implement Borrow, Clone or ToOwned usefully

    pub fn really_borrow(&'a self) -> Self {
        match self {
            &Value::Boolean(bool) => Value::Boolean(bool),
            &Value::Integer(integer) => Value::Integer(integer),
            &Value::String(ref string) => Value::String(Cow::Borrowed(string.borrow())),
            &Value::Entity(entity) => Value::Entity(entity),
        }
    }

    pub fn really_to_owned(&self) -> Value<'static> {
        match self {
            &Value::Boolean(bool) => Value::Boolean(bool),
            &Value::Integer(integer) => Value::Integer(integer),
            &Value::String(ref string) => Value::String(Cow::Owned(string.as_ref().to_owned())),
            &Value::Entity(entity) => Value::Entity(entity),
        }
    }
}

impl<'a> PartialEq<bool> for Value<'a> {
    fn eq(&self, other: &bool) -> bool {
        match self {
            &Value::Boolean(ref this) if this == other => true,
            _ => false,
        }
    }
}

impl<'a> PartialEq<i64> for Value<'a> {
    fn eq(&self, other: &i64) -> bool {
        match self {
            &Value::Integer(ref this) if this == other => true,
            _ => false,
        }
    }
}

impl<'a> PartialEq<str> for Value<'a> {
    fn eq(&self, other: &str) -> bool {
        match self {
            &Value::String(ref this) if this == other => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct Relation {
    pub columns: Vec<Vec<Value<'static>>>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct DB {
    pub relations: HashMap<String, Relation>,
}

fn parse_chinook_field(field: &str) -> Value<'static> {
    match field.parse::<i64>() {
        Ok(i) => Value::Integer(i),
        Err(_) => Value::String(Cow::Owned(field.to_owned())),
    }
}

pub fn load_chinook() -> Result<DB, Box<Error>> {
    let mut relations: HashMap<String, Relation> = HashMap::new();
    for name in vec![
        "Album",
        "Customer",
        "Genre",
        "InvoiceLine",
        "MediaType",
        "PlaylistTrack",
        "Artist",
        "Employee",
        "Invoice",
        "Playlist",
        "Track",
    ]
    {
        let mut reader = ::csv::ReaderBuilder::new().delimiter(b'\t').from_reader(
            File::open(
                format!(
                    "./data/{}.csv",
                    name
                ),
            )?,
        );
        let mut columns: Vec<Vec<Value>> = reader.headers()?.iter().map(|_| vec![]).collect();
        for record_or_error in reader.records() {
            let record = record_or_error?;
            for (c, field) in record.iter().enumerate() {
                columns[c].push(parse_chinook_field(field));
            }
        }
        relations.insert(name.to_lowercase(), Relation { columns });
    }
    Ok(DB { relations })
}

#[derive(Debug, Clone)]
pub enum Function {
    Add(usize, usize),
}

impl Function {
    pub fn apply<'a>(&self, variables: &[Value<'a>]) -> Result<Value<'static>, String> {
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

pub type RowCol = (usize, usize);

#[derive(Debug, Clone)]
pub enum Constraint {
    Join(usize, bool, Vec<RowCol>),
    Apply(usize, bool, Function),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub row_names: Vec<String>,
    pub row_orderings: Vec<Vec<usize>>,
    pub variables: Vec<Value<'static>>,
    pub constraints: Vec<Constraint>,
    pub result_vars: Vec<(String, usize)>,
}

impl ExprAst {
    fn is_constant(&self) -> bool {
        match self {
            &ExprAst::Constant(_) => true,
            _ => false,
        }
    }

    fn is_variable(&self) -> bool {
        match self {
            &ExprAst::Variable(_) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            &ExprAst::Relation(ref name, _) => {
                match &**name {
                    "+" => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

pub fn upsert<'a, T: Eq + Hash>(
    t_ix: &mut HashMap<&'a T, usize>,
    ts: &mut Vec<&'a T>,
    t: &'a T,
) -> usize {
    if let Some(&ix) = t_ix.get(&t) {
        ix
    } else {
        let ix = ts.len();
        ts.push(t);
        t_ix.insert(t, ix);
        ix
    }
}

enum Node<'a> {
    Statement(&'a StatementAst),
    Expr(&'a ExprAst),
}

pub fn plan(block: &BlockAst) -> Result<Block, String> {

    // flatten tree
    let mut expr_ix: HashMap<&ExprAst, usize> = HashMap::new();
    let mut exprs: Vec<&ExprAst> = Vec::new();
    let mut eq_exprs: HashSet<(&ExprAst, &ExprAst)> = HashSet::new();
    let mut top_level_exprs: HashSet<&ExprAst> = HashSet::new();
    let mut remaining_nodes = vec![Node::Statement(&block.body)];
    while let Some(node) = remaining_nodes.pop() {
        match node {
            Node::Statement(&StatementAst::Expr(ref expr)) => {
                top_level_exprs.insert(expr);
                upsert(&mut expr_ix, &mut exprs, expr);
                remaining_nodes.push(Node::Expr(expr));
            }
            Node::Statement(&StatementAst::Equals(ref expr1, ref expr2)) => {
                upsert(&mut expr_ix, &mut exprs, expr1);
                upsert(&mut expr_ix, &mut exprs, expr2);
                remaining_nodes.push(Node::Expr(expr1));
                remaining_nodes.push(Node::Expr(expr2));
                eq_exprs.insert((expr1, expr2));
            }
            Node::Statement(&StatementAst::Conjunction(ref statements)) => {
                for statement in statements.iter() {
                    remaining_nodes.push(Node::Statement(statement));
                }
            }
            Node::Expr(&ExprAst::Constant(_)) => (),
            Node::Expr(&ExprAst::Variable(_)) => (),
            Node::Expr(&ExprAst::Relation(_, ref args)) => {
                for arg in args.iter() {
                    upsert(&mut expr_ix, &mut exprs, arg);
                    remaining_nodes.push(Node::Expr(arg));
                }
            }
        }
    }

    // group exprs that must be equal
    let mut expr_group: HashMap<&ExprAst, usize> = expr_ix
        .iter()
        .filter(|&(e, _)| !top_level_exprs.contains(e))
        .map(|(&e, &i)| (e, i))
        .collect();
    for &(expr1, expr2) in eq_exprs.iter() {
        let group1 = *expr_group.get(expr1).unwrap();
        let group2 = *expr_group.get(expr2).unwrap();
        for (_, group) in expr_group.iter_mut() {
            if *group == group2 {
                *group = group1;
            }
        }
    }

    // gather up groups
    let mut group_exprs: HashMap<usize, Vec<&ExprAst>> = HashMap::new();
    for (expr, group) in expr_group.iter() {
        group_exprs.entry(*group).or_insert_with(|| vec![]).push(
            expr,
        );
    }

    // sort by order of appearance in code
    let mut slot_exprs: Vec<Vec<&ExprAst>> = group_exprs
        .iter()
        .map(|(_, exprs)| {
            let mut exprs = exprs.clone();
            exprs.sort_unstable_by_key(|e| expr_ix.get(e));
            exprs
        })
        .collect();
    slot_exprs.sort_unstable_by_key(|exprs| expr_ix.get(exprs[0]));

    // move slots that contain constants and no functions to the start
    for slot in 0..slot_exprs.len() {
        if slot_exprs[slot].iter().any(|e| e.is_constant()) &&
            slot_exprs[slot].iter().all(|e| !e.is_function())
        {
            let exprs = slot_exprs.remove(slot);
            slot_exprs.insert(0, exprs);
        }
    }

    // index in the other direction
    let mut expr_slot: HashMap<&ExprAst, usize> = HashMap::new();
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        for expr in exprs.iter() {
            expr_slot.insert(expr, slot);
        }
    }

    // collect exprs that directly query the database
    let mut row_names: Vec<String> = vec![];
    let mut row_exprs: Vec<Vec<&ExprAst>> = vec![];
    for &expr in exprs.iter() {
        match expr {
            &ExprAst::Relation(ref name, ref args) if !expr.is_function() => {
                let mut args: Vec<&ExprAst> = args.iter().collect();
                if !top_level_exprs.contains(expr) {
                    args.push(expr); // the value attached to the row
                }
                row_names.push(name.clone());
                row_exprs.push(args);
            }
            _ => (),
        }
    }

    // choose row indexes with columns in the order they appear in the slots
    let row_orderings: Vec<Vec<usize>> = row_exprs
        .iter()
        .map(|exprs| {
            let mut ordering: Vec<usize> = (0..exprs.len()).collect();
            ordering.sort_unstable_by_key(|&ix| expr_slot.get(exprs[ix]));
            ordering
        })
        .collect();

    println!("{:?}\n", slot_exprs);

    // produce constraints
    let mut values: Vec<Value> = (0..slot_exprs.len())
        .map(|_| Value::Boolean(false))
        .collect();
    let mut constraints: Vec<Constraint> = vec![];
    for (slot, exprs) in slot_exprs.iter().enumerate() {
        // gather up everything that constrains this slot
        let constants: Vec<&ExprAst> = exprs
            .iter()
            .map(|&e| e)
            .filter(|e| e.is_constant())
            .collect();
        let functions: Vec<&ExprAst> = exprs
            .iter()
            .map(|&e| e)
            .filter(|e| e.is_function())
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
            return Err(format!("No constraints on slot {}", slot)); // TODO how to report?
        }

        // after first constant or function, the rest just have to check their result is equal
        let mut slot_fixed_yet = false;

        // constants just go in the values vec
        if constants.len() > 0 {
            values[slot] = match constants[0] {
                &ExprAst::Constant(ref value) => value.clone(),
                _ => unreachable!(),
            };
            slot_fixed_yet = true;
        }

        // functions get run next
        for function in functions.iter() {
            match function {
                &&ExprAst::Relation(ref name, ref args) => {
                    let function = match (&**name, &**args) {
                        ("+", &[ref a, ref b]) => {
                            Function::Add(*expr_slot.get(a).unwrap(), *expr_slot.get(b).unwrap())
                        }
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
        if let Some(&ExprAst::Variable(ref name)) =
            exprs
                .iter()
                .map(|&expr| expr)
                .filter(|expr| expr.is_variable())
                .next()
        {
            result_vars.push((name.clone(), slot));
        }
    }

    Ok(Block {
        row_names,
        row_orderings,
        variables: values,
        constraints,
        result_vars,
    })
}

#[derive(Debug, Clone)]
pub struct CodeAst {
    pub blocks: Vec<Result<BlockAst, String>>,
    pub focused: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct BlockAst {
    pub body: StatementAst,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ExprAst {
    Constant(Value<'static>),
    Variable(String),
    Relation(String, Vec<ExprAst>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum StatementAst {
    Expr(ExprAst),
    Equals(ExprAst, ExprAst),
    Conjunction(Vec<StatementAst>),
}

pub fn simplify_errors<Output>(
    result: IResult<&[u8], Output>,
    input: &str,
) -> Result<Output, String> {
    match result {
        IResult::Done(remaining, output) => {
            if remaining.len() == 0 {
                Ok(output)
            } else {
                Err(format!(
                    "Remaining: {}\n\n{:?}",
                    ::std::str::from_utf8(remaining).unwrap(),
                    input
                ))
            }
        }
        IResult::Error(error) => Err(format!("Nom error: {:?}\n\n{:?}", error, input)),
        IResult::Incomplete(needed) => Err(format!("Nom incomplete: {:?}\n\n{:?}", needed, input)),
    }
}

pub fn code_ast(text: &str, cursor: i64) -> CodeAst {
    let blocks = text.split("\n\n")
        .map(|block| {
            let block = format!("{}\n", block); // hacky way to get nom to stop streaming
            simplify_errors(block_ast(block.as_bytes()), &*block)
        })
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

named!(block_ast(&[u8]) -> BlockAst, map!(statement_ast, |e| BlockAst{body: e}));

named!(statement_ast(&[u8]) -> StatementAst, map!(
    many0!(do_parse!(
        s: simple_statement_ast >>
            opt!(space) >>
            tag!("\n") >>
    (s))),
    StatementAst::Conjunction
));

named!(simple_statement_ast(&[u8]) -> StatementAst, do_parse!(
    e: expr_ast >>
        equals: opt!(equals_ast) >>
        ({
            if let Some(e2) = equals {
                StatementAst::Equals(e, e2)
            } else {
                StatementAst::Expr(e)
            }
        })
));

named!(equals_ast(&[u8]) -> ExprAst, do_parse!(
    opt!(space) >>
    tag!("=") >>
    opt!(space) >>
    e: expr_ast >>
    (e)
));

named!(expr_ast(&[u8]) -> ExprAst, do_parse!(
    e: simple_expr_ast >>
        f: opt!(infix_function_ast) >>
        ({
            if let Some((name, arg)) = f {
                ExprAst::Relation(name, vec![e, arg])
            } else {
                e
            }
        })
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

named!(simple_expr_ast(&[u8]) -> ExprAst, alt!(
        map!(relation_ast, |(name, args)| ExprAst::Relation(name, args)) |
        map!(value_ast, ExprAst::Constant) |
        map!(symbol_ast, ExprAst::Variable) |
        paren_ast
));

named!(relation_ast(&[u8]) -> (String, Vec<ExprAst>), do_parse!(
        name: symbol_ast >>
        tag!("(") >>
        args: separated_list_complete!(tuple!(opt!(space), tag!(","), opt!(space)), expr_ast) >>
        tag!(")") >>
        (name, args)
));

named!(value_ast(&[u8]) -> Value<'static>, alt!(
    map!(integer_ast, Value::Integer) |
    map!(boolean_ast, Value::Boolean) |
    map!(string_ast, |s| Value::String(Cow::Owned(s)))
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

named!(symbol_ast(&[u8]) -> String, map_res!(
    verify!(
        take_while1_s!(|c| is_alphanumeric(c) || c == ('-' as u8)),
        |b: &[u8]| is_alphabetic(b[0])
    ),
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
