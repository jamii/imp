#![feature(box_syntax)]
#![feature(box_patterns)]

use log::{error, info, warn};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt;
use std::iter::FromIterator;

#[macro_use]
mod macros;
mod lower;
mod syntax;

pub use syntax::parse;

pub type Name = String; // non-empty

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
// TODO Eq/Ord for fn are dubious - should implement on name instead
pub struct Native {
    name: Name,
    input_arity: usize,
    output_arity: usize,
    fun: fn(Vec<Scalar>) -> Result<Value, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Nothing,
    Something,
    Scalar(Scalar),
    Union(Box<Expression>, Box<Expression>),
    Intersect(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Negate(Box<Expression>),
    Name(Name),
    Let(Name, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    ApplyNative(Native, Vec<Name>),
    Seal(Box<Expression>),
    Unseal(Box<Expression>),
    Exists(Vec<Name>, Box<Expression>),
    Solve(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scalar {
    String(String),
    Number(i64),
    Sealed(Box<Expression>, Environment<Value>),
}

pub type Set = BTreeSet<Vec<Scalar>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Set(Set),
    Closure(Name, Expression, Environment<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    Any,
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Nothing,
    Something,
    Product(ScalarType, Box<ValueType>), // no Abstract inside Product
    Abstract(ScalarType, Box<ValueType>),
}

#[derive(Debug, Clone)]
pub enum Arity {
    Exactly(usize),
    AtLeast(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment<T> {
    bindings: Vec<(Name, T)>,
}

#[derive(Debug, Clone)]
pub struct Cache<T> {
    cache: HashMap<*const Expression, T>,
}

#[derive(Debug, Clone)]
enum LowerAction {
    Refer(String, Vec<Name>),
    Inline(Expression),
}
#[derive(Debug, Clone)]
pub struct Scope {
    vars: Vec<Option<Name>>,
}

enum Lowered {}

impl<T> Cache<T> {
    pub fn new() -> Self {
        Cache {
            cache: HashMap::new(),
        }
    }

    fn insert(&mut self, expr: &Expression, value: T) {
        self.cache.insert(expr as *const Expression, value);
    }

    pub fn get(&self, expr: &Expression) -> &T {
        self.cache
            .get(&(expr as *const Expression))
            .unwrap_or_else(|| panic!("Not in cache: {:?}", expr))
    }
}

impl Scope {
    fn push(&mut self, var: Option<Name>) {
        self.vars.push(var);
    }

    fn resolve(&mut self, var: &Name) -> Option<usize> {
        self.vars.iter().position(|var2| var2.as_ref() == Some(var))
    }
}

fn write_delimited<T, TS: IntoIterator<Item = T>, F: Fn(&mut fmt::Formatter, T) -> fmt::Result>(
    f: &mut fmt::Formatter,
    delimiter: &str,
    things: TS,
    write: F,
) -> fmt::Result {
    let mut iter = things.into_iter().enumerate().peekable();
    while let Some((_i, thing)) = iter.next() {
        write(f, thing)?;
        if let Some(_) = iter.peek() {
            write!(f, "{}", delimiter)?;
        }
    }
    Ok(())
}

fn write_environment<T>(f: &mut fmt::Formatter, env: &Environment<T>) -> fmt::Result
where
    T: std::fmt::Display,
{
    for (var, value) in &env.bindings {
        write!(f, "let {} = {} in ", var, value)?;
    }
    Ok(())
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Scalar::String(string) => write!(f, "{:?}", string)?,
            Scalar::Number(number) => write!(f, "{:?}", number)?,
            Scalar::Sealed(expr, env) => {
                write!(f, "{{")?;
                write_environment(f, env)?;
                write!(f, "{}", expr)?;
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            v if v.is_nothing() => write!(f, "nothing")?,
            v if v.is_something() => write!(f, "something")?,
            Value::Set(set) => {
                let is_something = self.is_something();
                if is_something {
                    write!(f, "(")?;
                }
                write_delimited(f, " | ", set, |f, value| {
                    write_delimited(f, " x ", value, |f, scalar| write!(f, "{}", scalar))
                })?;
                if is_something {
                    write!(f, ")")?;
                }
            }
            Value::Closure(name, body, env) => {
                write!(f, "({} -> ", name)?;
                write_environment(f, env)?;
                write!(f, "{})", body)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Nothing => write!(f, "nothing")?,
            Something => write!(f, "something")?,
            Scalar(scalar) => write!(f, "{}", scalar)?,
            Union(e1, e2) => write!(f, "({} | {})", e1, e2)?,
            Intersect(e1, e2) => write!(f, "({} & {})", e1, e2)?,
            Product(e1, e2) => write!(f, "({} x {})", e1, e2)?,
            Equal(e1, e2) => write!(f, "({} = {})", e1, e2)?,
            Negate(e) => write!(f, "!{}", e)?,
            Name(name) => write!(f, "{}", name)?,
            Let(name, value, body) => write!(f, "let {} = {} in {}", name, value, body)?,
            If(cond, if_true, if_false) => {
                write!(f, "if {} then {} else {}", cond, if_true, if_false)?
            }
            Abstract(arg, body) => {
                write!(f, "({} -> {})", arg, body)?;
            }
            Apply(fun, arg) => write!(f, "({} {})", fun, arg)?,
            ApplyNative(fun, args) => {
                write!(f, "(<{}> ", fun.name)?;
                write_delimited(f, " ", args, |f, arg| write!(f, "{}", arg))?;
                write!(f, ")")?;
            }
            Seal(e) => write!(f, "{{{}}}", e)?,
            Unseal(e) => write!(f, "${}", e)?,
            Exists(args, body) => {
                write!(f, "exists(")?;
                write_delimited(f, " ", args, |f, arg| write!(f, "{}", arg))?;
                write!(f, " -> {})", body)?;
            }
            Solve(e) => {
                write!(f, "?({})", e)?;
            }
        }
        Ok(())
    }
}

impl ScalarType {
    fn union(self, _other: Self) -> Result<Self, String> {
        Ok(ScalarType::Any)
    }

    fn intersect(self, _other: Self) -> Result<Self, String> {
        Ok(ScalarType::Any)
    }

    fn product(self, tail: ValueType) -> ValueType {
        if tail.is_function() {
            ValueType::Abstract(self, box tail)
        } else {
            ValueType::Product(self, box tail)
        }
    }

    fn abstract_(self, tail: ValueType) -> ValueType {
        ValueType::Abstract(self, box tail)
    }
}

impl ValueType {
    fn is_function(&self) -> bool {
        match self {
            ValueType::Abstract(..) => true,
            _ => false,
        }
    }

    fn arity(&self) -> Arity {
        match self {
            ValueType::Nothing => Arity::AtLeast(0),
            ValueType::Something => Arity::Exactly(0),
            ValueType::Product(_, tail) | ValueType::Abstract(_, tail) => match tail.arity() {
                Arity::AtLeast(a) => Arity::AtLeast(a + 1),
                Arity::Exactly(a) => Arity::Exactly(a + 1),
            },
        }
    }

    fn union(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, t) | (t, Nothing) => t,
            (Something, Something) => Something,
            (Product(s1, t1), Product(s2, t2)) => Product(s1.union(s2)?, box t1.union(*t2)?),
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => Abstract(s1.union(s2)?, box t1.union(*t2)?),
            (t1, t2) => return Err(format!("Can't unify {:?} and {:?}", t1, t2)),
        })
    }

    fn intersect(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, _) | (_, Nothing) => Nothing,
            (Something, Something) => Something,
            (Product(s1, t1), Product(s2, t2)) => {
                Product(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (Abstract(s1, t1), Abstract(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                Abstract(s1.intersect(s2)?, box t1.intersect(*t2)?)
            }
            (t1, t2) => return Err(format!("Can't unify {:?} and {:?}", t1, t2)),
        })
    }

    fn product(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match self {
            Nothing => Nothing,
            Something => match other {
                Abstract(..) => return Err(format!("Product of function: {:?}", other)),
                _ => other,
            },
            Product(s1, t1) => ValueType::Product(s1, box t1.product(other)?),
            Abstract(..) => return Err(format!("Product of function: {:?}", self)),
        })
    }

    fn apply(self, other: Self) -> Result<Self, String> {
        use ValueType::*;
        Ok(match (self, other) {
            (Nothing, _) | (_, Nothing) => Nothing,
            (Something, t) | (t, Something) => t,
            (Product(s1, t1), Product(s2, t2))
            | (Abstract(s1, t1), Product(s2, t2))
            | (Product(s1, t1), Abstract(s2, t2)) => {
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                s1.intersect(s2)?;
                t1.apply(*t2)?
            }
            (t1 @ Abstract(..), t2 @ Abstract(..)) => {
                return Err(format!("Applied function to function: {:?} {:?}", t1, t2));
            }
        })
    }

    fn negate(self) -> Self {
        use ValueType::*;
        match self {
            Nothing | Something => Something, // the type Something includes the values something and nothing. kinda weird
            Product(s, t) | Abstract(s, t) => Abstract(s, box t.negate()),
        }
    }

    fn solve(self) -> Self {
        use ValueType::*;
        match self {
            Nothing => Nothing,
            Something => Something,
            Product(s, t) | Abstract(s, t) => Product(s, box t.solve()),
        }
    }
}

impl Scalar {
    fn as_integer(&self) -> Result<i64, String> {
        match self {
            Scalar::Number(i) => Ok(*i),
            _ => Err(format!("Not an integer: {}", self)),
        }
    }

    fn unseal(&self) -> Result<Value, String> {
        match self {
            Scalar::Sealed(expr, env) => expr.clone().eval(&env),
            _ => Err(format!("${}", self)),
        }
    }
}

impl Native {
    fn add(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1), Scalar::Number(n2)] => Ok(Value::scalar(Scalar::Number(n1 + n2))),
            [a, b] => Err(format!("{} + {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn permute(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [permutations @ Scalar::Sealed(..), set @ Scalar::Sealed(..)] => {
                match (permutations.unseal()?, set.unseal()?) {
                    (Value::Set(permutations), Value::Set(set)) => {
                        let mut result = Set::new();
                        for permutation in permutations {
                            for row in &set {
                                result.insert(
                                    permutation
                                        .iter()
                                        .map(|i| {
                                            let i = i.as_integer()? - 1;
                                            if 0 <= i && i < (row.len() as i64) {
                                                Ok(row[i as usize].clone())
                                            } else {
                                                Err(format!("Out of bounds: {}", i + 1))
                                            }
                                        })
                                        .collect::<Result<Vec<_>, _>>()?,
                                );
                            }
                        }
                        Ok(Value::Set(result))
                    }
                    (a, b) => Err(format!("permute {{{}}} {{{}}}", a, b)),
                }
            }
            [a, b] => Err(format!("permute {} {}", a, b)),
            _ => unreachable!(),
        }
    }

    fn reduce(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [init, fun @ Scalar::Sealed(..), set @ Scalar::Sealed(..)] => {
                match (fun.unseal()?, set.unseal()?) {
                    (fun, Value::Set(set)) => {
                        let mut result = Value::Set(Set::from_iter(vec![vec![init.clone()]]));
                        for row in set {
                            let env = Environment::from(vec![
                                ("fun".to_owned(), fun.clone()),
                                ("result".to_owned(), result),
                                ("row".to_owned(), Value::Set(Set::from_iter(vec![row]))),
                            ]);
                            let expr = Expression::Apply(
                                box Expression::Apply(
                                    box Expression::Name("fun".to_owned()),
                                    box Expression::Name("result".to_owned()),
                                ),
                                box Expression::Name("row".to_owned()),
                            );
                            result = expr.eval(&env)?;
                        }
                        Ok(result)
                    }
                    (a, b) => Err(format!("reduce {} {{{}}} {{{}}}", init, a, b)),
                }
            }
            [a, b, c] => Err(format!("reduce {} {} {}", a, b, c)),
            _ => unreachable!(),
        }
    }

    // TODO pivot isn't quite the right name for this
    fn pivot(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [set @ Scalar::Sealed(..)] => match set.unseal()? {
                Value::Set(set) => {
                    let mut result = Set::new();
                    let mut input = set.into_iter().collect::<Vec<_>>();
                    input.sort();
                    for (r, row) in input.into_iter().enumerate() {
                        for (c, val) in row.into_iter().enumerate() {
                            result.insert(vec![
                                Scalar::Number(r as i64),
                                Scalar::Number(c as i64),
                                val,
                            ]);
                        }
                    }
                    Ok(Value::Set(result))
                }
                a => Err(format!("pivot {{{}}}", a)),
            },
            [a] => Err(format!("pivot {}", a)),
            _ => unreachable!(),
        }
    }

    fn as_text(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [scalar] => Ok(Value::Set(Set::from_iter(vec![vec![Scalar::String(
                format!("{}", scalar),
            )]]))),
            _ => unreachable!(),
        }
    }

    // fn solve(scalars: Vec<Scalar>) -> Result<Value, String> {
    //     match &*scalars {
    //         [Scalar::Sealed(box expr, env)] => {
    //             // TODO scalar_env?
    //             let scalar_env = Environment::from(
    //                 env.bindings
    //                     .iter()
    //                     .map(|(name, _value)| (name.clone(), false))
    //                     .collect::<Vec<(String, bool)>>(),
    //             );
    //             let type_env = Environment::from(
    //                 env.bindings
    //                     .iter()
    //                     .map(|(name, value)| Ok((name.clone(), value.typ()?)))
    //                     .collect::<Result<Vec<(String, ValueType)>, String>>()?,
    //             );
    //             let mut scalar_cache = Cache::new();
    //             let mut type_cache = Cache::new();
    //             expr.scalar(&scalar_env, &mut scalar_cache)?;
    //             expr.typecheck(&type_env, &mut type_cache)?;
    //             let lowered = expr.lower(&scalar_cache, &type_cache)?;
    //             Ok(Value::Set(Set::from_iter(vec![vec![Scalar::Sealed(
    //                 box lowered,
    //                 env.clone(),
    //             )]])))
    //         }
    //         [a] => Err(format!("solve {}", a)),
    //         _ => unreachable!(),
    //     }
    // }

    pub fn stdlib() -> Vec<Native> {
        vec![
            Native {
                name: "+".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::add,
            },
            Native {
                name: "permute".to_owned(),
                input_arity: 2,
                output_arity: 1,
                fun: Native::permute,
            },
            Native {
                name: "reduce".to_owned(),
                input_arity: 3,
                output_arity: 1,
                fun: Native::reduce,
            },
            Native {
                name: "pivot".to_owned(),
                input_arity: 1,
                output_arity: 3,
                fun: Native::pivot,
            },
            Native {
                name: "as_text".to_owned(),
                input_arity: 1,
                output_arity: 1,
                fun: Native::as_text,
            },
            // Native {
            //     name: "solve".to_owned(),
            //     input_arity: 1,
            //     output_arity: 1,
            //     fun: Native::solve,
            // },
        ]
    }
}

impl<T> Environment<T> {
    pub fn new() -> Self {
        Environment { bindings: vec![] }
    }

    fn from(bindings: Vec<(Name, T)>) -> Self {
        Environment { bindings: bindings }
    }

    fn lookup(&self, name: &Name) -> Option<&T> {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, e)| e)
    }

    fn bind(&mut self, name: Name, value: T) {
        self.bindings.push((name, value));
    }
}

impl<T> Environment<T>
where
    T: Clone,
{
    fn close_over(&self, names: BTreeSet<Name>) -> Self {
        Environment::from(
            self.bindings
                .iter()
                .filter(|(name, _)| names.contains(name))
                .cloned()
                .collect(),
        )
    }
}

impl Value {
    fn set<T: IntoIterator<Item = Vec<Scalar>>>(tuples: T) -> Value {
        Value::Set(tuples.into_iter().collect())
    }

    fn nothing() -> Value {
        Value::set(vec![])
    }

    fn something() -> Value {
        Value::set(vec![vec![]])
    }

    fn scalar(scalar: Scalar) -> Value {
        Value::set(vec![vec![scalar]])
    }

    fn is_nothing(&self) -> bool {
        match self {
            Value::Set(set) if set.len() == 0 => true,
            _ => false,
        }
    }

    fn is_something(&self) -> bool {
        match self {
            Value::Set(set) if set.len() == 1 => set.iter().next().unwrap().is_empty(),
            _ => false,
        }
    }

    fn as_scalar(&self) -> Result<Scalar, String> {
        if let Value::Set(set) = self {
            if set.len() == 1 {
                let tuple = set.iter().next().unwrap();
                if tuple.len() == 1 {
                    let scalar = tuple.iter().next().unwrap();
                    return Ok(scalar.clone());
                }
            }
        }
        Err(format!("Not a scalar: {}", self))
    }

    fn union(val1: Value, val2: Value) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(if val1.is_nothing() {
            val2
        } else if val2.is_nothing() {
            val1
        } else {
            match (val1, val2) {
                (Set(set1), Set(set2)) => Value::set(set1.union(&set2).cloned()),
                (v1, v2) => {
                    // (f | g) => (a -> (f a) | (g a))
                    let env = Environment::from(vec![("v1".to_owned(), v1), ("v2".to_owned(), v2)]);
                    let expr = Union(
                        box Apply(box Name("v1".to_owned()), box Name("a".to_owned())),
                        box Apply(box Name("v2".to_owned()), box Name("a".to_owned())),
                    );
                    Closure("a".to_owned(), expr, env)
                }
            }
        })
    }

    fn intersect(val1: Value, val2: Value) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(if val1.is_nothing() {
            Value::nothing()
        } else if val2.is_nothing() {
            Value::nothing()
        } else {
            match (val1, val2) {
                (Set(set1), Set(set2)) => Value::set(set1.intersection(&set2).cloned()),
                (v1, v2) => {
                    // (f & g) => (a -> (f a) & (g a))
                    let env = Environment::from(vec![("v1".to_owned(), v1), ("v2".to_owned(), v2)]);
                    let expr = Intersect(
                        box Apply(box Name("v1".to_owned()), box Name("a".to_owned())),
                        box Apply(box Name("v2".to_owned()), box Name("a".to_owned())),
                    );
                    Closure("a".to_owned(), expr, env)
                }
            }
        })
    }

    fn product(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => Value::set(set1.iter().flat_map(|tuple1| {
                set2.iter().map(move |tuple2| {
                    let mut tuple = tuple1.clone();
                    tuple.extend_from_slice(tuple2);
                    tuple
                })
            })),
            (v1, v2) => Err(format!("Type error: {} , {}", v1, v2))?,
        })
    }

    fn equals(val1: Value, val2: Value) -> Value {
        if val1 == val2 {
            Value::something()
        } else {
            Value::nothing()
        }
    }

    fn negation(val: Value) -> Value {
        use Expression::*;
        use Value::*;
        if val.is_nothing() {
            Value::something()
        } else if val.is_something() {
            Value::nothing()
        } else {
            // !f => (a -> !(f a))
            Closure(
                "a".to_owned(),
                Negate(box Apply(
                    box Name("v".to_owned()),
                    box Name("a".to_owned()),
                )),
                Environment::from(vec![("v".to_owned(), val)]),
            )
        }
    }

    fn apply(val1: Value, val2: Value) -> Result<Value, String> {
        use Value::*;
        Ok(match (val1, val2) {
            (Set(set1), Set(set2)) => {
                let mut tuples = vec![];
                for tuple1 in &set1 {
                    for tuple2 in &set2 {
                        if tuple2.starts_with(&tuple1) {
                            tuples.push(tuple2[tuple1.len()..].to_vec());
                        } else if tuple1.starts_with(&tuple2) {
                            tuples.push(tuple1[tuple2.len()..].to_vec());
                        }
                    }
                }
                Value::set(tuples)
            }
            (Closure(name, body, env), Set(set)) | (Set(set), Closure(name, body, env)) => {
                let mut set_iter = set.into_iter();
                match set_iter.next() {
                    None => Value::nothing(),
                    Some(tuple) => {
                        // fun (scalar x tuple | tail) => (fun scalar tuple | fun tail)
                        let head = {
                            let mut tuple_iter = tuple.into_iter();
                            match tuple_iter.next() {
                                None => Closure(name.clone(), body.clone(), env.clone()),
                                Some(scalar) => {
                                    let tuple = tuple_iter.collect::<Vec<_>>();
                                    let mut new_env = env.clone();
                                    new_env.bind(name.clone(), Value::scalar(scalar));
                                    Value::apply(
                                        body.clone().eval(&new_env)?,
                                        Value::set(vec![tuple]),
                                    )?
                                }
                            }
                        };
                        let tail = Value::apply(Closure(name, body, env), Value::set(set_iter))?;
                        Value::union(head, tail)?
                    }
                }
            }
            (v1, v2) => Err(format!("Type error: {} {}", v1, v2))?,
        })
    }

    fn unseal(val: Value) -> Result<Value, String> {
        val.as_scalar()?.unseal()
    }

    fn typ(&self) -> Result<ValueType, String> {
        // TODO this method is fishy - should seal capture checked types instead? might mess with staging
        use Value::*;
        Ok(match self {
            Set(set) => {
                let mut arities = set
                    .iter()
                    .map(|row| row.len())
                    .collect::<HashSet<_>>()
                    .into_iter();
                match (arities.next(), arities.next()) {
                    (None, None) => ValueType::Nothing,
                    (Some(arity), None) => {
                        let mut typ = ValueType::Something;
                        for _ in 0..arity {
                            typ = ValueType::Product(ScalarType::Any, box ValueType::Something);
                        }
                        typ
                    }
                    (_, Some(_)) => return Err(format!("Mismatched arities in: {}", self)),
                }
            }
            Closure(name, body, env) => {
                let mut type_env = Environment::from(
                    env.bindings
                        .iter()
                        .map(|(name, value)| Ok((name.clone(), value.typ()?)))
                        .collect::<Result<Vec<(String, ValueType)>, String>>()?,
                );
                type_env.bind(
                    name.clone(),
                    ValueType::Product(ScalarType::Any, box ValueType::Something),
                );
                let mut type_cache = Cache::new();
                body.typecheck(&type_env, &mut type_cache)?
            }
        })
    }
}

impl Expression {
    fn _abstract(mut args: Vec<Name>, body: Expression) -> Expression {
        args.reverse();
        args.into_iter()
            .fold(body, |body, arg| Expression::Abstract(arg, box body))
    }

    fn apply(fun: &str, args: Vec<Expression>) -> Expression {
        args.into_iter()
            .fold(Expression::Name(fun.to_owned()), |fun, arg| {
                Expression::Apply(box fun, box arg)
            })
    }

    fn visit1<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            Nothing => (),
            Something => (),
            Scalar(_) => (),
            Union(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Intersect(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Product(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Equal(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Negate(e) => f(&*e)?,
            Name(_) => (),
            Let(_, value, body) => {
                f(&*value)?;
                f(&*body)?;
            }
            If(cond, if_true, if_false) => {
                f(&*cond)?;
                f(&*if_true)?;
                f(&*if_false)?;
            }
            Abstract(_, body) => f(&*body)?,
            Apply(fun, arg) => {
                f(&*fun)?;
                f(&*arg)?;
            }
            ApplyNative(_, _) => (),
            Seal(e) => f(&*e)?,
            Unseal(e) => f(&*e)?,
            Exists(_, body) => f(&*body)?,
            Solve(e) => f(&*e)?,
        }
        Ok(())
    }

    fn visit<'a, F>(&'a self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&'a Expression) -> Result<(), String>,
    {
        self.visit1(&mut |e| e.visit(f))?;
        f(self)
    }

    fn visit1_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        use Expression::*;
        match self {
            Nothing => (),
            Something => (),
            Scalar(_) => (),
            Union(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Intersect(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Product(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Equal(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Negate(box e) => f(e)?,
            Name(_) => (),
            Let(_, box value, box body) => {
                f(value)?;
                f(body)?;
            }
            If(box cond, box if_true, box if_false) => {
                f(cond)?;
                f(if_true)?;
                f(if_false)?;
            }
            Abstract(_, box body) => f(body)?,
            Apply(box fun, box arg) => {
                f(fun)?;
                f(arg)?;
            }
            ApplyNative(_, _) => (),
            Seal(box e) => f(e)?,
            Unseal(box e) => f(e)?,
            Exists(_, box body) => f(body)?,
            Solve(box e) => f(e)?,
        }
        Ok(())
    }

    fn visit_mut<F>(&mut self, f: &mut F) -> Result<(), String>
    where
        F: FnMut(&mut Expression) -> Result<(), String>,
    {
        self.visit1_mut(&mut |e| e.visit_mut(f))?;
        f(self)
    }

    fn map1<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit1_mut(&mut |e| {
            *e = f(std::mem::replace(e, Expression::Nothing))?;
            Ok(())
        })?;
        Ok(self)
    }

    fn map<F>(mut self, mut f: F) -> Result<Self, String>
    where
        F: FnMut(Expression) -> Result<Expression, String>,
    {
        self.visit_mut(&mut |e| {
            *e = f(std::mem::replace(e, Expression::Nothing))?;
            Ok(())
        })?;
        Ok(self)
    }

    pub fn desugar(self) -> Expression {
        // use Expression::*;
        // match self.map(|e| e.desugar()) {
        //     Let(name, value, body) => Apply(box Abstract(name, body), value),
        //     other => other,
        // }
        self
    }

    pub fn with_natives(self, natives: &[Native]) -> Expression {
        fn map(
            expr: Expression,
            natives: &BTreeMap<Name, &Native>,
            bound: &BTreeSet<Name>,
        ) -> Expression {
            use Expression::*;
            match expr {
                Name(name) => {
                    if let (false, Some(native)) = (bound.contains(&name), natives.get(&name)) {
                        let args = (0..native.input_arity)
                            .map(|i| format!("a{}", i))
                            .collect::<Vec<_>>();
                        Expression::_abstract(args.clone(), ApplyNative((*native).clone(), args))
                    } else {
                        Name(name)
                    }
                }
                Abstract(arg, box body) => {
                    let mut bound = bound.clone();
                    bound.insert(arg.clone());
                    Abstract(arg, box map(body, natives, &bound))
                }
                expr => expr.map1(|e| Ok(map(e, natives, bound))).unwrap(),
            }
        }
        let natives = BTreeMap::from_iter(natives.into_iter().map(|n| (n.name.clone(), n)));
        map(self, &natives, &BTreeSet::new())
    }

    pub fn free_names(&self) -> BTreeSet<Name> {
        fn visit(expr: &Expression, free_names: &mut BTreeSet<Name>, bound: &BTreeSet<Name>) {
            match expr {
                Expression::Name(name) => {
                    if !bound.contains(name) {
                        free_names.insert(name.clone());
                    }
                }
                Expression::ApplyNative(_, args) => {
                    for arg in args {
                        if !bound.contains(arg) {
                            free_names.insert(arg.clone());
                        }
                    }
                }
                Expression::Abstract(name, body) => {
                    let mut bound = bound.clone();
                    bound.insert(name.clone());
                    visit(body, free_names, &bound);
                }
                _ => {
                    expr.visit1(&mut |e| {
                        visit(e, free_names, bound);
                        Ok(())
                    })
                    .unwrap();
                }
            }
        }
        let mut free_names = BTreeSet::new();
        visit(self, &mut free_names, &BTreeSet::new());
        free_names
    }

    pub fn eval(self, env: &Environment<Value>) -> Result<Value, String> {
        use Expression::*;
        use Value::*;
        Ok(match self {
            Nothing => Value::nothing(),
            Something => Value::something(),
            Scalar(scalar) => Value::scalar(scalar),
            Union(box e1, box e2) => Value::union(e1.eval(env)?, e2.eval(env)?)?,
            Intersect(box e1, box e2) => Value::intersect(e1.eval(env)?, e2.eval(env)?)?,
            Product(box e1, box e2) => Value::product(e1.eval(env)?, e2.eval(env)?)?,
            Equal(box e1, box e2) => Value::equals(e1.eval(env)?, e2.eval(env)?),
            Negate(box e) => Value::negation(e.eval(env)?),
            Name(name) => match env.lookup(&name) {
                Some(value) => value.clone(),
                None => Err(format!("Undefined: {}", name))?,
            },
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name, value.eval(&env)?);
                body.eval(&env)?
            }
            If(box cond, box if_true, box if_false) => {
                if !cond.eval(env)?.is_nothing() {
                    if_true.eval(env)?
                } else {
                    if_false.eval(env)?
                }
            }
            Abstract(arg, box body) => {
                let closure_env = env.close_over(body.free_names());
                Closure(arg, body, closure_env)
            }
            Apply(fun, arg) => Value::apply(fun.eval(env)?, arg.eval(env)?)?,
            ApplyNative(native, args) => (native.fun)(
                args.into_iter()
                    // with_natives wraps ApplyNative in a function so by this point we know all the args exist and are scalars
                    .map(|arg| env.lookup(&arg).unwrap().clone().as_scalar().unwrap())
                    .collect(),
            )?,
            Seal(e) => {
                let seal_env = env.close_over(e.free_names());
                Value::scalar(crate::Scalar::Sealed(e, seal_env))
            }
            Unseal(e) => Value::unseal(e.eval(env)?)?,
            Exists(..) | Solve(..) => return Err(format!("Unimplemented: {}", self)),
        })
    }

    pub fn scalar(&self, env: &Environment<bool>, cache: &mut Cache<bool>) -> Result<bool, String> {
        use Expression::*;
        let scalar = match self {
            Scalar(_) => true,
            Name(name) => env
                .lookup(name)
                .ok_or_else(|| format!("Unbound name: {}", name))?
                .clone(),
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name.clone(), value.scalar(&env, cache)?);
                body.scalar(&env, cache)?
            }
            Exists(names, box body) => {
                let mut env = env.clone();
                for name in names {
                    env.bind(name.clone(), true);
                }
                body.scalar(&env, cache)?
            }
            Abstract(arg, body) => {
                let mut env = env.clone();
                env.bind(arg.clone(), true);
                body.scalar(&env, cache)?
            }
            _ => {
                self.visit1(&mut |e| {
                    e.scalar(env, cache)?;
                    Ok(())
                })?;
                false
            }
        };
        cache.insert(self, scalar);
        Ok(scalar)
    }

    pub fn typecheck(
        &self,
        env: &Environment<ValueType>,
        cache: &mut Cache<ValueType>,
    ) -> Result<ValueType, String> {
        use Expression::*;
        let typ = match self {
            Nothing => ValueType::Nothing,
            Something => ValueType::Something,
            Scalar(_) => ValueType::Product(ScalarType::Any, box ValueType::Something),
            Union(e1, e2) => e1.typecheck(env, cache)?.union(e2.typecheck(env, cache)?)?,
            Intersect(e1, e2) => e1
                .typecheck(env, cache)?
                .intersect(e2.typecheck(env, cache)?)?,
            Product(e1, e2) => e1
                .typecheck(env, cache)?
                .product(e2.typecheck(env, cache)?)?,
            Equal(e1, e2) => {
                // TODO intersect is fine for now, but may want to think more carefully about this once we have real scalar types
                e1.typecheck(env, cache)?
                    .intersect(e2.typecheck(env, cache)?)?;
                ValueType::Something
            }
            Negate(e) => e.typecheck(env, cache)?.negate(),
            Name(name) => env
                .lookup(name)
                .ok_or_else(|| format!("Unbound variable: {:?}", name))?
                .clone(),
            Let(name, value, body) => {
                let mut env = env.clone();
                env.bind(name.clone(), value.typecheck(&env, cache)?);
                body.typecheck(&env, cache)?
            }
            If(c, t, f) => {
                match c.typecheck(env, cache)? {
                    ValueType::Nothing | ValueType::Something => (),
                    other => return Err(format!("Non-boolean condition in `if`: {:?}", other)),
                }
                t.typecheck(env, cache)?.union(f.typecheck(env, cache)?)?
            }
            Abstract(arg, body) => {
                let mut env = env.clone();
                env.bind(
                    arg.clone(),
                    ValueType::Product(ScalarType::Any, box ValueType::Something),
                );
                ValueType::Abstract(ScalarType::Any, box body.typecheck(&env, cache)?)
            }
            Apply(e1, e2) => e1.typecheck(env, cache)?.apply(e2.typecheck(env, cache)?)?,
            ApplyNative(f, args) => {
                assert_eq!(f.input_arity, args.len());
                let mut t = ValueType::Something;
                for _ in 0..f.output_arity {
                    t = ValueType::Product(ScalarType::Any, box t)
                }
                t
            }
            Seal(e) => {
                e.typecheck(env, cache)?;
                ValueType::Product(ScalarType::Any, box ValueType::Something)
            }
            Unseal(..) => return Err(format!("Can't type unseal")),
            Exists(..) => return Err(format!("Can't type exists")),
            Solve(e) => e.typecheck(env, cache)?.solve(),
        };
        cache.insert(self, typ.clone());
        Ok(typ)
    }

    pub fn rename(self, old: &Name, new: &Name) -> Self {
        use Expression::*;
        match self {
            Name(name) => {
                if name == *old {
                    Name(new.clone())
                } else {
                    Name(name)
                }
            }
            Let(name, value, body) => {
                let body = if name == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Let(name, box value.rename(old, new), body)
            }
            Abstract(arg, body) => {
                let body = if arg == *old {
                    body
                } else {
                    box body.rename(old, new)
                };
                Abstract(arg, body)
            }
            ApplyNative(f, mut args) => {
                for arg in &mut args {
                    if arg == old {
                        *arg = new.clone();
                    }
                }
                ApplyNative(f, args)
            }
            _ => self.map1(|expr| Ok(expr.rename(old, new))).unwrap(),
        }
    }

    fn contains(
        &self,
        args: &[Name],
        actions: &Environment<LowerAction>,
        scalar_cache: &Cache<bool>,
        type_cache: &Cache<ValueType>,
        next_tmp: &mut usize,
        lets: &mut Vec<(Name, Vec<Name>, Expression)>,
    ) -> Result<Self, String> {
        use Expression::*;
        Ok(match self {
            Nothing => Nothing,
            Something => {
                assert_eq!(args.len(), 0);
                Something
            }
            Scalar(scalar) => {
                assert_eq!(args.len(), 1);
                Apply(box Scalar(scalar.clone()), box Name(args[0].clone()))
            }
            Union(box e1, box e2) => Union(
                box e1.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box e2.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Intersect(box e1, box e2) => Intersect(
                box e1.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box e2.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Product(box e1, box e2) => {
                let a1 = match type_cache.get(e1).arity() {
                    Arity::Exactly(a) => a,
                    Arity::AtLeast(_) => 0,
                };
                Intersect(
                    box e1.contains(
                        &args[0..a1],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?,
                    box e2.contains(
                        &args[a1..],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?,
                )
            }
            Equal(box e1, box e2) => {
                assert_eq!(args.len(), 0);
                Equal(box e1.clone(), box e2.clone())
            }
            Negate(box e) => {
                Negate(box e.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?)
            }
            Name(name) => match actions.lookup(name) {
                Some(LowerAction::Inline(expr)) => {
                    expr.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?
                }
                // TODO might need to be careful about shadowing here if variable names are not unique
                Some(LowerAction::Refer(new_name, prefix_args)) => {
                    // TODO this will have the wrong var names
                    let expr = Expression::apply(
                        new_name,
                        prefix_args.iter().map(|arg| Name(arg.clone())).collect(),
                    );
                    expr
                }
                None => Err(format!("Name from outside of lower {:?}", name))?,
            },
            Let(name, value, box body) => {
                let mut actions = actions.clone();
                match type_cache.get(&value) {
                    ValueType::Abstract(..) => {
                        actions.bind(name.clone(), LowerAction::Inline((**value).clone()));
                        body.contains(args, &actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    typ => {
                        let arity = match typ.arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(a) => a,
                        };
                        let value_args = (0..arity)
                            .map(|_| {
                                let name = format!("var{}", next_tmp);
                                *next_tmp += 1;
                                name
                            })
                            .collect::<Vec<_>>();
                        let value = value.contains(
                            &value_args,
                            &actions,
                            scalar_cache,
                            type_cache,
                            next_tmp,
                            lets,
                        )?;
                        let new_name = format!("{}_{}", name, next_tmp);
                        *next_tmp += 1;
                        lets.push((new_name.clone(), args.to_vec(), value));
                        actions.bind(name.clone(), LowerAction::Refer(new_name, args.to_vec()));
                        body.contains(args, &actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                }
            }
            If(box cond, box if_true, box if_false) => If(
                box cond.contains(&[], actions, scalar_cache, type_cache, next_tmp, lets)?,
                box if_true.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
                box if_false.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?,
            ),
            Abstract(arg, box body) => {
                assert!(args.len() >= 1); // TODO otherwise replace with nothing
                let expr = body
                    .contains(
                        &args[1..],
                        actions,
                        scalar_cache,
                        type_cache,
                        next_tmp,
                        lets,
                    )?
                    // TODO does this assume arg is unique?
                    .rename(arg, &args[0]);
                expr
            }
            Apply(box left, box right) => {
                match (*scalar_cache.get(left), *scalar_cache.get(right)) {
                    (true, true) => {
                        assert_eq!(args.len(), 0);
                        Equal(box left.clone(), box right.clone())
                    }
                    (true, false) => {
                        let mut args = args.to_vec();
                        let left_name = match left {
                            Name(name) => name,
                            _ => unreachable!(),
                        };
                        args.insert(0, left_name.clone());
                        right.contains(&args, actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    (false, true) => {
                        let mut args = args.to_vec();
                        let right_name = match right {
                            Name(name) => name,
                            _ => unreachable!(),
                        };
                        args.insert(0, right_name.clone());
                        left.contains(&args, actions, scalar_cache, type_cache, next_tmp, lets)?
                    }
                    (false, false) => {
                        let left_arity = match type_cache.get(left).arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(_) => 0,
                        };
                        let right_arity = match type_cache.get(right).arity() {
                            Arity::Exactly(a) => a,
                            Arity::AtLeast(_) => 0,
                        };
                        let new_arg_names = (0..left_arity.min(right_arity))
                            .map(|_| {
                                let name = format!("extra_var{}", next_tmp);
                                *next_tmp += 1;
                                name
                            })
                            .collect::<Vec<_>>();
                        let mut left_args = new_arg_names.clone();
                        let mut right_args = new_arg_names;
                        if left_arity < right_arity {
                            right_args.extend_from_slice(args);
                        } else {
                            left_args.extend_from_slice(args);
                        }
                        Intersect(
                            box left.contains(
                                &left_args,
                                actions,
                                scalar_cache,
                                type_cache,
                                next_tmp,
                                lets,
                            )?,
                            box right.contains(
                                &right_args,
                                actions,
                                scalar_cache,
                                type_cache,
                                next_tmp,
                                lets,
                            )?,
                        )
                    }
                }
            }
            ApplyNative(native, native_args) => {
                assert_eq!(args.len(), native.output_arity);
                ApplyNative(native.clone(), native_args.clone())
            }
            // Seal(e) => {
            //     assert_eq!(args.len(), 1);
            //     Apply(box Seal(e.clone()), box Name(args[0].clone()))
            // }
            Solve(e) => {
                // TODO check e is finite here (or elsewhere)
                e.contains(args, actions, scalar_cache, type_cache, next_tmp, lets)?
            }
            _ => return Err(format!("Can't lower {}", self)),
        })
    }

    // fn applied_to(&self, outer: Lowered, outer_scope: Scope) -> Result<Lowered, String> {
    //     use Expression::*;
    //     use Lowered::*;
    //     match self {}
    // }

    pub fn lower(
        &self,
        scalar_cache: &Cache<bool>,
        type_cache: &Cache<ValueType>,
    ) -> Result<Vec<(Name, Vec<Name>, Expression)>, String> {
        let scope = Environment::new();
        let mut next_tmp = 0;
        let arity = match type_cache.get(self).arity() {
            Arity::Exactly(a) => a,
            Arity::AtLeast(_) => 0,
        };
        let args = (0..arity)
            .map(|_| {
                let name = format!("var{}", next_tmp);
                next_tmp += 1;
                name
            })
            .collect::<Vec<_>>();
        let mut lets = vec![];
        let predicate = self.contains(
            &args,
            &scope,
            scalar_cache,
            type_cache,
            &mut next_tmp,
            &mut lets,
        )?;
        lets.push((format!("result{}", next_tmp), args, predicate));
        Ok(lets)
    }

    // pub fn reorder(
    //     self,
    //     ordering: &[Expression],
    //     indexes: &mut Vec<(Name, Expression, Vec<usize>)>,
    //     next_tmp: &mut usize,
    // ) -> Expression {
    //     use Expression::*;
    //     if let Apply(..) = self {
    //         let mut f = self;
    //         let mut args: Vec<Expression> = vec![];
    //         while let Apply(box e1, box e2) = f {
    //             f = e1;
    //             args.insert(0, e2.clone());
    //         }
    //         let name = {
    //             let name = format!("index{}", next_tmp);
    //             *next_tmp += 1;
    //             name
    //         };
    //         let mut permutation = args.into_iter().enumerate().collect::<Vec<_>>();
    //         permutation.sort_by_key(|(_, arg)| ordering.iter().position(|o| o == arg));
    //         let (permutation, args) = permutation.into_iter().unzip();
    //         indexes.push((name.clone(), f, permutation));
    //         Expression::apply(&name, args)
    //     } else {
    //         self.map1(|e| Ok(e.reorder(lets, indexes, next_tmp)))
    //             .unwrap()
    //     }
    // }

    // pub fn bound(&self, name: &Name, type_cache: &Cache<ValueType>) -> Option<Expression> {
    //     use Expression::*;
    //     match self {
    //         Intersect(e1, e2) => {
    //             match (e1.bound(name, type_cache), e2.bound(name, type_cache)) {
    //                 (Some(b1), Some(b2)) => Some(Intersect(box b1, box b2)),
    //                 (Some(b), None) | (None, Some(b)) => Some(b),
    //                 (None, None) => None,
    //             }
    //         }
    //         Apply(box f, box arg) => {
    //             if let Name(arg) = arg {
    //                 if arg == name {
    //                     // TODO want to check
    //                     // && !type_cache.get(f).is_function() {
    //                     // but we don't have types for indexes because we permuted them
    //                     return Some(
    //                         Apply(
    //                             box Apply(
    //                                 box Name("permute".to_owned()),
    //                                 box Seal(box Scalar(crate::Scalar::Number(1))),
    //                             ),
    //                             box Seal(box f.clone()),
    //                         )
    //                         .with_natives(&Native::stdlib()),
    //                     );
    //                 }
    //             }
    //             f.bound(name, type_cache)
    //         }
    //         Equal(box e1, box e2) => {
    //             if let Name(e1) = e1 {
    //                 if e1 == name {
    //                     return Some(e2.clone());
    //                 }
    //             }
    //             if let Name(e2) = e2 {
    //                 if e2 == name {
    //                     return Some(e1.clone());
    //                 }
    //             }
    //             None
    //         }
    //         Exists(_, box e) => e.bound(name, type_cache),
    //         _ => None,
    //     }
    // }

    // pub fn lower2(
    //     &self,
    //     scalar_cache: &Cache<bool>,
    //     type_cache: &Cache<ValueType>,
    // ) -> Result<Self, String> {
    //     use Expression::*;
    //     let mut next_tmp = 0;
    //     let arity = match type_cache.get(self).arity() {
    //         Arity::Exactly(a) => a,
    //         Arity::AtLeast(_) => 0,
    //     };
    //     let arg_names = (0..arity)
    //         .map(|_| {
    //             let name = format!("var{}", next_tmp);
    //             next_tmp += 1;
    //             name
    //         })
    //         .collect::<Vec<_>>();
    //     let args = arg_names
    //         .iter()
    //         .map(|name| Name(name.clone()))
    //         .collect::<Vec<_>>();
    //     let mut ordering = vec![];
    //     let predicate = self.contains(&args, scalar_cache, type_cache, &mut next_tmp, &mut lets)?;
    //     {
    //         let mut seen = HashSet::new();
    //         ordering.retain(|name| seen.insert(name.clone()));
    //     }
    //     let mut indexes = vec![];
    //     let predicate = predicate.reorder(
    //         &ordering
    //             .iter()
    //             .map(|name| Name(name.clone()))
    //             .collect::<Vec<_>>(),
    //         &mut indexes,
    //         &mut next_tmp,
    //     );
    //     let mut expr = If(
    //         box predicate.clone(),
    //         box args
    //             .into_iter()
    //             .fold(Something, |a, b| Product(box a, box b)),
    //         box Nothing,
    //     );
    //     for name in ordering.iter().rev() {
    //         expr = Apply(
    //             box predicate
    //                 .bound(name, type_cache)
    //                 .unwrap_or(Name("panic".to_owned())),
    //             box Abstract(name.clone(), box expr),
    //         );
    //     }
    //     for (name, source, permutation) in indexes.into_iter().rev() {
    //         expr = Let(
    //             name,
    //             box Apply(
    //                 box Apply(
    //                     box Abstract(
    //                         "tmp0".to_owned(),
    //                         box Abstract(
    //                             "tmp1".to_owned(),
    //                             box ApplyNative(
    //                                 Native {
    //                                     name: "permute".to_owned(),
    //                                     input_arity: 2,
    //                                     output_arity: 1,
    //                                     fun: Native::permute,
    //                                 },
    //                                 vec!["tmp0".to_owned(), "tmp1".to_owned()],
    //                             ),
    //                         ),
    //                     ),
    //                     box Seal(box permutation.into_iter().fold(Something, |expr, i| {
    //                         Product(box expr, box Scalar(self::Scalar::Number((i + 1) as i64)))
    //                     })),
    //                 ),
    //                 box Seal(box source),
    //             ),
    //             box expr,
    //         );
    //     }
    //     Ok(expr)
    // }
}

pub fn eval(expr: Expression) -> Result<Value, String> {
    expr.eval(&Environment::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(code: &str) -> Result<Value, String> {
        parse(code)
            .map_err(|error| format!("{:?}", error))?
            .desugar()
            .with_natives(&Native::stdlib())
            .eval(&Environment::new())
    }

    macro_rules! assert_eq_run {
            ( $code1:expr, $($code2:expr),* $(,)? ) => {{
                $(
                    println!("{} = {}, {:?} = {:?}", $code1, $code2, run($code1), run($code2));
                    assert_eq!(run($code1), run($code2), "{:?} = {:?}", $code1, $code2);
                )+
            }};
        }

    #[test]
    fn basic() {
        assert_eq!(
            parse("a").unwrap().free_names(),
            vec!["a".to_owned()].into_iter().collect()
        );
        assert_eq!(
            parse("a | b").unwrap().free_names(),
            vec!["a".to_owned(), "b".to_owned()].into_iter().collect()
        );
        assert_eq!(
            parse("a -> a | b").unwrap().free_names(),
            vec!["b".to_owned()].into_iter().collect()
        );

        assert_eq_run!("(1 | 2) + 3", "3 + (1 | 2)", "4 | 5");

        // assert_eq!(
        //     run("1"),
        //     Expression::Materialized(Relation::from_iter(vec![vec![Value::Number(1)]]))
        // );
        // assert_eq!(
        //     run(r#""foo""#),
        //     Expression::Materialized(Relation::from_iter(vec![vec![Value::String(
        //         "foo".to_owned()
        //     )]]))
        // );

        // assert_eq_run!("()", "something");
        // assert_eq_run!("(nothing,)", "nothing");
        // assert_eq_run!("(nothing,something)", "nothing");
        // assert_eq_run!("(something,something)", "something");
        // assert_eq_run!("(1,(2,3))", "((1,2),3)", "(1,2,3)");

        // assert_eq_run!("!nothing", "something");
        // assert_eq_run!("!something", "nothing");
        // assert_eq_run!("!3", "nothing");

        // assert_eq_run!("nothing | something", "something");
        // assert_eq_run!("nothing & something", "nothing");
        // assert_eq_run!("(1 | 2) & (2 | 3)", "2");
        // assert_eq_run!("1 | (2 | 3)", "(1 | 2) | 3");

        // assert_eq_run!("1 = 1", "something");
        // assert_eq_run!("1 = 2", "nothing");
        // assert_eq_run!("(1 | 2) = (2 | 1)", "something");

        // assert_eq_run!("1[1]", "something");
        // assert_eq_run!("1[2]", "nothing");
        // assert_eq_run!(r#"((1, "one") | (2, "two"))[2]"#, r#""two""#);

        // // assert!(run("(1, _, 3)"))
        // assert_eq_run!("(1, 2, _, 3, 4)", "((1, 2), _, 3, 4)");

        // //         run("
        // // let fixpoint_loop = f => old => new => if old = new then new else fixpoint_loop{f, new, f{old}} in
        // // let fixpoint = f => old => fixpoint_loop{f, new, f{old}} in
        // // fixpoint{x => x, 1}
        // // ");
    }
}
