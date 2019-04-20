#![feature(box_syntax)]
#![feature(box_patterns)]

use lalrpop_util::lalrpop_mod;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
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
    arity: u64,
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
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    ApplyNative(Native, Vec<Name>),
    Seal(Box<Expression>),
    Unseal(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scalar {
    String(String),
    Number(i64),
    Sealed(Box<Expression>, Environment),
}

pub type Set = BTreeSet<Vec<Scalar>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Set(Set),
    Closure(Name, Expression, Environment),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment {
    bindings: Vec<(Name, Value)>,
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

fn write_environment(f: &mut fmt::Formatter, env: &Environment) -> fmt::Result {
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
            Unseal(e) => write!(f, "~{}", e)?,
        }
        Ok(())
    }
}

impl Native {
    fn add(scalars: Vec<Scalar>) -> Result<Value, String> {
        match &*scalars {
            [Scalar::Number(n1), Scalar::Number(n2)] => Ok(Value::scalar(Scalar::Number(n1 + n2))),
            _ => unimplemented!(),
        }
    }

    pub fn stdlib() -> Vec<Native> {
        vec![Native {
            name: "+".to_owned(),
            arity: 2,
            fun: Native::add,
        }]
    }
}

impl Environment {
    fn new() -> Environment {
        Environment { bindings: vec![] }
    }

    fn from(bindings: Vec<(Name, Value)>) -> Environment {
        Environment { bindings: bindings }
    }

    fn lookup(&self, name: &Name) -> Option<&Value> {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, e)| e)
    }

    fn bind(&mut self, name: Name, value: Value) {
        self.bindings.push((name, value));
    }

    fn close_over(&self, names: BTreeSet<Name>) -> Environment {
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

    fn as_scalar(&self) -> Option<Scalar> {
        if let Value::Set(set) = self {
            if set.len() == 1 {
                let tuple = set.iter().next().unwrap();
                if tuple.len() == 1 {
                    let scalar = tuple.iter().next().unwrap();
                    return Some(scalar.clone());
                }
            }
        }
        None
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
                        // fun (scalar, tuple | tail) => (fun scalar, tuple | fun tail)
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
        match val.as_scalar() {
            Some(Scalar::Sealed(expr, env)) => expr.clone().eval(&env),
            _ => Err(format!("~{}", val)),
        }
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

    fn visit<F: FnMut(&Expression)>(&self, f: &mut F) {
        use Expression::*;
        match self {
            Nothing => (),
            Something => (),
            Scalar(_) => (),
            Union(box e1, box e2) => {
                f(e1);
                f(e2);
            }
            Intersection(box e1, box e2) => {
                f(e1);
                f(e2);
            }
            Product(box e1, box e2) => {
                f(e1);
                f(e2);
            }
            Equals(box e1, box e2) => {
                f(e1);
                f(e2);
            }
            Negation(box e) => f(e),
            Name(_) => (),
            Let(name, box value, box body) => {
                f(value);
                f(body);
            }
            Abstract(_, box body) => f(body),
            Apply(box fun, box arg) => {
                f(fun);
                f(arg);
            }
            ApplyNative(_, _) => (),
            Seal(box e) => f(e),
            Unseal(box e) => f(e),
        }
    }

    fn map<F: FnMut(Expression) -> Expression>(self, mut f: F) -> Expression {
        use Expression::*;
        match self {
            Nothing => Nothing,
            Something => Something,
            Scalar(scalar) => Scalar(scalar),
            Union(box e1, box e2) => Union(box f(e1), box f(e2)),
            Intersection(box e1, box e2) => Intersection(box f(e1), box f(e2)),
            Product(box e1, box e2) => Product(box f(e1), box f(e2)),
            Equals(box e1, box e2) => Equals(box f(e1), box f(e2)),
            Negation(box e) => Negation(box f(e)),
            Name(name) => Name(name),
            Let(name, box value, box body) => Let(name, box f(value), box f(body)),
            Abstract(arg, box body) => Abstract(arg, box f(body)),
            Apply(box fun, box arg) => Apply(box f(fun), box f(arg)),
            ApplyNative(fun, args) => ApplyNative(fun, args),
            Seal(box e) => Seal(box f(e)),
            Unseal(box e) => Unseal(box f(e)),
        }
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
        fn mapper(
            expr: Expression,
            natives: &BTreeMap<Name, &Native>,
            bound: &BTreeSet<Name>,
        ) -> Expression {
            use Expression::*;
            match expr {
                Name(name) => {
                    if let (false, Some(native)) = (bound.contains(&name), natives.get(&name)) {
                        let args = (0..native.arity)
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
                    Abstract(arg, box mapper(body, natives, &bound))
                }
                expr => expr.map(|e| mapper(e, natives, bound)),
            }
        }
        let natives = BTreeMap::from_iter(natives.into_iter().map(|n| (n.name.clone(), n)));
        mapper(self, &natives, &BTreeSet::new())
    }

    pub fn free_names(&self) -> BTreeSet<Name> {
        fn mapper(expr: &Expression, free_names: &mut BTreeSet<Name>, bound: &BTreeSet<Name>) {
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
                    mapper(body, free_names, &bound);
                }
                _ => {
                    expr.visit(&mut |e| mapper(e, free_names, bound));
                }
            }
        }
        let mut free_names = BTreeSet::new();
        mapper(self, &mut free_names, &BTreeSet::new());
        free_names
    }

    pub fn eval(self, env: &Environment) -> Result<Value, String> {
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
        })
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
