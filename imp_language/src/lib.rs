#![feature(box_syntax)]
#![feature(box_patterns)]

use lalrpop_util::lalrpop_mod;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt;
use std::iter::FromIterator;

// macro_rules! iflet {
//     ( $p:pat = $e:expr ) => {{
//         if let $o = $e {
//             true
//         } else {
//             false
//         }
//     }};
// }

// macro_rules! s {
//     ( $l:literal ) => {{
//         $l.to_owned()
//     }};
// }

lalrpop_mod!(pub syntax);

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
    Intersection(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
    Negation(Box<Expression>),
    Name(Name),
    Let(Name, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    ApplyNative(Native, Vec<Name>),
    Seal(Box<Expression>),
    Unseal(Box<Expression>),
    Exists(Vec<Name>, Box<Expression>),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment<T> {
    bindings: Vec<(Name, T)>,
}

#[derive(Debug, Clone)]
pub struct Cache<T> {
    cache: HashMap<*const Expression, T>,
}

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
                if set.len() > 1 || (set.len() == 1 && set.iter().next().unwrap().len() > 1) {
                    write!(f, "(")?;
                }
                write_delimited(f, " | ", set, |f, value| {
                    write_delimited(f, " x ", value, |f, scalar| write!(f, "{}", scalar))
                })?;
                if set.len() > 1 || (set.len() == 1 && set.iter().next().unwrap().len() > 1) {
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
            Intersection(e1, e2) => write!(f, "({} & {})", e1, e2)?,
            Product(e1, e2) => write!(f, "({} x {})", e1, e2)?,
            Equals(e1, e2) => write!(f, "({} = {})", e1, e2)?,
            Negation(e) => write!(f, "!{}", e)?,
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
        }
        Ok(())
    }
}

fn gensym() -> Name {
    format!("tmp_{}", rand::random::<usize>())
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
                                                Err(format!("Out of bounds: {}", i))
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

    fn solve(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Sealed(box expr, env)] => {
                // TODO scalar_env?
                let scalar_env = Environment::from(
                    env.bindings
                        .iter()
                        .map(|(name, value)| (name.clone(), false))
                        .collect::<Vec<(String, bool)>>(),
                );
                let arity_env = Environment::from(
                    env.bindings
                        .iter()
                        .map(|(name, value)| Ok((name.clone(), value.arity()?)))
                        .collect::<Result<Vec<(String, Option<usize>)>, String>>()?,
                );
                let mut scalar_cache = Cache::new();
                let mut arity_cache = Cache::new();
                expr.scalar(&scalar_env, &mut scalar_cache).unwrap();
                expr.arity(&arity_env, &mut arity_cache).unwrap();
                let lowered = expr.lower(&scalar_cache, &arity_cache).unwrap();
                Ok(Value::Set(Set::from_iter(vec![vec![Scalar::Sealed(
                    box lowered,
                    env.clone(),
                )]])))
            }
            [a] => Err(format!("solve {}", a)),
            _ => unreachable!(),
        }
    }

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
                name: "solve".to_owned(),
                input_arity: 1,
                output_arity: 1,
                fun: Native::solve,
            },
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

    fn intersection(val1: Value, val2: Value) -> Result<Value, String> {
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
                    let expr = Intersection(
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
                Negation(box Apply(
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

    fn arity(&self) -> Result<Option<usize>, String> {
        use Value::*;
        Ok(match self {
            Set(set) => {
                let mut arities = set
                    .iter()
                    .map(|row| row.len())
                    .collect::<HashSet<_>>()
                    .into_iter();
                match (arities.next(), arities.next()) {
                    (None, None) => None,
                    (Some(a), None) => Some(a),
                    (_, Some(_)) => return Err(format!("Mismatched arities in: {}", self)),
                }
            }
            Closure(name, body, env) => {
                let mut arity_env = Environment::from(
                    env.bindings
                        .iter()
                        .map(|(name, value)| Ok((name.clone(), value.arity()?)))
                        .collect::<Result<Vec<(String, Option<usize>)>, String>>()?,
                );
                arity_env.bind(name.clone(), Some(1));
                let mut arity_cache = Cache::new();
                body.arity(&arity_env, &mut arity_cache)?.map(|a| a + 1)
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
            Intersection(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Product(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Equals(e1, e2) => {
                f(&*e1)?;
                f(&*e2)?;
            }
            Negation(e) => f(&*e)?,
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
            Intersection(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Product(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Equals(box e1, box e2) => {
                f(e1)?;
                f(e2)?;
            }
            Negation(box e) => f(e)?,
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
            Intersection(box e1, box e2) => Value::intersection(e1.eval(env)?, e2.eval(env)?)?,
            Product(box e1, box e2) => Value::product(e1.eval(env)?, e2.eval(env)?)?,
            Equals(box e1, box e2) => Value::equals(e1.eval(env)?, e2.eval(env)?),
            Negation(box e) => Value::negation(e.eval(env)?),
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
            Exists(_, _) => return Err(format!("Unimplemented: {}", self)),
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

    pub fn arity(
        &self,
        env: &Environment<Option<usize>>,
        cache: &mut Cache<Option<usize>>,
    ) -> Result<Option<usize>, String> {
        use Expression::*;
        fn unify(a1: Option<usize>, a2: Option<usize>) -> Result<Option<usize>, String> {
            match (a1, a2) {
                (None, None) => Ok(None),
                (Some(a), None) | (None, Some(a)) => Ok(Some(a)),
                (Some(a1), Some(a2)) => {
                    if a1 == a2 {
                        Ok(Some(a1))
                    } else {
                        Err(format!("Can't unify arities {} and {}", a1, a2))
                    }
                }
            }
        }
        let arity = match self {
            Nothing => None,
            Something => Some(0),
            Scalar(_) => Some(1),
            Union(box e1, box e2) => unify(e1.arity(env, cache)?, e2.arity(env, cache)?)?,
            Intersection(box e1, box e2) => unify(e1.arity(env, cache)?, e2.arity(env, cache)?)?,
            Product(box e1, box e2) => match (e1.arity(env, cache)?, e2.arity(env, cache)?) {
                (Some(a1), Some(a2)) => Some(a1 + a2),
                _ => None,
            },
            Equals(box e1, box e2) => {
                e1.arity(env, cache)?;
                e2.arity(env, cache)?;
                Some(0)
            }
            Negation(box e) => e.arity(&env, cache)?,
            Name(name) => env
                .lookup(name)
                .ok_or_else(|| format!("Unbound name: {}", name))?
                .clone(),
            Let(name, box value, box body) => {
                let mut env = env.clone();
                env.bind(name.clone(), value.arity(&env, cache)?);
                body.arity(&env, cache)?
            }
            If(box cond, box if_true, box if_false) => {
                cond.arity(env, cache)?;
                unify(if_true.arity(env, cache)?, if_false.arity(env, cache)?)?
            }
            Abstract(arg, box body) => {
                let mut env = env.clone();
                env.bind(arg.clone(), Some(1));
                body.arity(&env, cache)?.map(|a| a + 1)
            }
            Apply(box fun, box arg) => match (fun.arity(env, cache)?, arg.arity(env, cache)?) {
                (Some(a1), Some(a2)) => Some(a1.max(a2) - a1.min(a2)),
                _ => None,
            },
            ApplyNative(native, args) => {
                assert_eq!(native.input_arity, args.len() as usize);
                Some(native.output_arity)
            }
            Seal(box e) => {
                e.arity(env, cache)?;
                Some(1)
            }
            Unseal(_) => return Err(format!("Can't infer arity of: {}", self)),
            Exists(names, box e) => {
                let mut env = env.clone();
                for name in names {
                    env.bind(name.clone(), Some(1));
                }
                e.arity(&env, cache)?;
                Some(0)
            }
        };
        cache.insert(self, arity);
        Ok(arity)
    }

    pub fn replace(self, old: &Name, new: &Expression) -> Self {
        use Expression::*;
        match self {
            Name(name) => {
                if name == *old {
                    new.clone()
                } else {
                    Name(name)
                }
            }
            Let(name, value, body) => {
                if name == *old {
                    Let(name, box value.replace(old, new), body)
                } else {
                    Let(
                        name,
                        box value.replace(old, new),
                        box body.replace(old, new),
                    )
                }
            }
            Abstract(arg, body) => {
                if arg == *old {
                    Abstract(arg, body)
                } else {
                    Abstract(arg, box body.replace(old, new))
                }
            }
            _ => self.map1(|expr| Ok(expr.replace(old, new))).unwrap(),
        }
    }

    pub fn contains(
        &self,
        args: &[Name],
        scalar_cache: &Cache<bool>,
        arity_cache: &Cache<Option<usize>>,
        next_tmp: &mut usize,
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
                box e1.contains(args, scalar_cache, arity_cache, next_tmp)?,
                box e2.contains(args, scalar_cache, arity_cache, next_tmp)?,
            ),
            Intersection(box e1, box e2) => Intersection(
                box e1.contains(args, scalar_cache, arity_cache, next_tmp)?,
                box e2.contains(args, scalar_cache, arity_cache, next_tmp)?,
            ),
            Product(box e1, box e2) => {
                let a1 = arity_cache.get(e1).unwrap_or(0);
                Intersection(
                    box e1.contains(&args[0..a1], scalar_cache, arity_cache, next_tmp)?,
                    box e2.contains(&args[a1..], scalar_cache, arity_cache, next_tmp)?,
                )
            }
            Equals(box e1, box e2) => {
                assert_eq!(args.len(), 0);
                if *scalar_cache.get(e1) && *scalar_cache.get(e2) {
                    Apply(box e1.clone(), box e2.clone())
                } else {
                    Equals(box e1.clone(), box e2.clone())
                }
            }
            Negation(box e) => {
                Negation(box e.contains(args, scalar_cache, arity_cache, next_tmp)?)
            }
            Name(name) => {
                Expression::apply(name, args.iter().map(|name| Name(name.clone())).collect())
            }
            // Let(name, box value, box body) => {
            //     // TODO this will make caches blow up
            //     // use an env instead?
            //     body.replace(name, value)
            //         .contains(args, scalar_cache, arity_cache, next_tmp)
            // }
            If(box cond, box if_true, box if_false) => If(
                box cond.contains(&[], scalar_cache, arity_cache, next_tmp)?,
                box if_true.contains(args, scalar_cache, arity_cache, next_tmp)?,
                box if_false.contains(args, scalar_cache, arity_cache, next_tmp)?,
            ),
            Abstract(arg, box body) => {
                assert!(args.len() >= 1);
                body.contains(&args[1..], scalar_cache, arity_cache, next_tmp)?
                    .replace(arg, &Name(args[0].clone()))
            }
            Apply(box left, box right) => {
                let left_arity = arity_cache.get(left).unwrap_or(0);
                let right_arity = arity_cache.get(right).unwrap_or(0);
                let new_args = (0..left_arity.min(right_arity))
                    .map(|_| {
                        let name = format!("tmp_{}", next_tmp);
                        *next_tmp += 1;
                        name
                    })
                    .collect::<Vec<_>>();
                let mut left_args = new_args.clone();
                let mut right_args = new_args.clone();
                if left_arity < right_arity {
                    right_args.extend_from_slice(args);
                } else {
                    left_args.extend_from_slice(args);
                }
                Exists(
                    new_args,
                    box Intersection(
                        box left.contains(&left_args, scalar_cache, arity_cache, next_tmp)?,
                        box right.contains(&right_args, scalar_cache, arity_cache, next_tmp)?,
                    ),
                )
            }
            ApplyNative(native, native_args) => {
                assert_eq!(args.len(), 0);
                ApplyNative(native.clone(), native_args.clone())
            }
            Seal(e) => {
                assert_eq!(args.len(), 1);
                Apply(box Seal(e.clone()), box Name(args[0].clone()))
            }
            _ => return Err(format!("Can't lower {}", self)),
        })
    }

    pub fn lower(
        &self,
        scalar_cache: &Cache<bool>,
        arity_cache: &Cache<Option<usize>>,
    ) -> Result<Self, String> {
        let mut next_tmp = 0;
        let arity = arity_cache.get(&self).unwrap_or(0);
        let args = (0..arity)
            .map(|_| {
                let name = format!("tmp_{}", next_tmp);
                next_tmp += 1;
                name
            })
            .collect::<Vec<_>>();
        Ok(Expression::_abstract(
            args.clone(),
            self.contains(&args, scalar_cache, arity_cache, &mut next_tmp)?,
        ))
    }
}

pub fn parse(code: &str) -> Result<Expression, String> {
    syntax::Expression0Parser::new()
        .parse(code)
        .map_err(|error| format!("{:?}", error))
}

pub fn eval(expr: Expression) -> Result<Value, String> {
    expr.eval(&Environment::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(code: &str) -> Result<Value, String> {
        parse(code)?
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
            parse("\\ a -> a | b").unwrap().free_names(),
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
