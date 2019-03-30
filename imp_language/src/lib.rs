#![feature(box_syntax)]
#![feature(box_patterns)]

use lalrpop_util::lalrpop_mod;
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

lalrpop_mod!(pub syntax);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    String(String),
    Number(i64),
    Set(BTreeSet<Vec<Value>>),
    Closure(Name, Box<Expression>, Environment),
}

pub type Environment = Vec<(Name, Value)>;

pub type Name = String; // non-empty

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Everything,
    Value(Value),
    Name(Name),
    Let(Name, Box<Expression>, Box<Expression>),
    Abstract(Name, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    Solve(Box<Expression>),
}

impl Expression {
    fn nothing() -> Self {
        Expression::Value(Value::Set(BTreeSet::from_iter(vec![])))
    }

    fn something() -> Self {
        Expression::Value(Value::Set(BTreeSet::from_iter(vec![vec![]])))
    }

    fn _abstract(mut args: Vec<Name>, body: Expression) -> Self {
        args.reverse();
        args.into_iter()
            .fold(body, |body, arg| Expression::Abstract(arg, box body))
    }

    fn apply(fun: Expression, args: Vec<Expression>) -> Self {
        args.into_iter()
            .fold(fun, |f, arg| Expression::Apply(box f, box arg))
    }

    fn primitive(name: &str, args: Vec<Expression>) -> Self {
        Expression::apply(Expression::Name(name.to_owned()), args)
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(string) => write!(f, "{:?}", string)?,
            Value::Number(number) => write!(f, "{:?}", number)?,
            Value::Set(set) => {
                write!(f, "{{")?;
                write_delimited(f, " | ", set, |f, vs| {
                    write_delimited(f, " x ", vs, |f, v| write!(f, "{}", v))
                })?;
                write!(f, "}}")?;
            }
            Value::Closure(name, body, env) => {
                write!(f, "(")?;
                for (name, value) in env {
                    write!(f, "let {} = {} in ", name, value)?;
                }
                write!(f, "\\ {} -> {}", name, body)?;
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Everything => write!(f, "everything")?,
            Value(value) => write!(f, "{}", value)?,
            Name(name) => {
                if (*name == "!")
                    || (*name == "=")
                    || (*name == "|")
                    || (*name == "&")
                    || (*name == "x")
                {
                    write!(f, "({})", name)?;
                } else {
                    write!(f, "{}", name)?;
                }
            }
            Abstract(arg, body) => {
                write!(f, "({} -> {})", arg, body)?;
            }
            // arity 1 primitives
            Apply(box Name(name), arg) if (*name == "!") => write!(f, "{}{}", name, arg)?,
            // arity 2 primitives
            Apply(box Apply(box Name(name), arg1), arg2)
                if (*name == "=") || (*name == "|") || (*name == "&") || (*name == "x") =>
            {
                write!(f, "({} {} {})", arg1, name, arg2)?
            }
            Apply(fun, arg) => write!(f, "({} {})", fun, arg)?,
            Solve(def) => write!(f, "{{{}}}", def)?,

            Let(name, value, body) => write!(f, "let {} = {} in {}", name, value, body)?,
        }
        Ok(())
    }
}

impl Expression {
    fn map<F: FnMut(Expression) -> Expression>(self, mut f: F) -> Expression {
        use Expression::*;
        match self {
            Everything => Everything,
            Value(value) => Value(value),
            Name(name) => Name(name),
            Abstract(arg, box body) => Abstract(arg, box f(body)),
            Apply(box fun, box arg) => Apply(box f(fun), box f(arg)),
            Solve(box def) => Solve(box f(def)),

            Let(name, box value, box body) => Let(name, box f(value), box f(body)),
        }
    }

    fn desugar(self) -> Self {
        use Expression::*;
        match self.map(|e| e.desugar()) {
            Let(name, value, body) => Apply(box Abstract(name, value), body),
            other => other,
        }
    }

    fn evaluate(self, env: &Environment, everything: &Option<Vec<Value>>) -> Result<Value, String> {
        use crate::Value::*;
        use Expression::*;
        Ok(match self {
            Everything => {
                if let Some(everything) = everything {
                    Set(BTreeSet::from_iter(
                        everything.clone().into_iter().map(|e| vec![e]),
                    ))
                } else {
                    Err(format!("everything"))?
                }
            }
            Value(value) => value,
            Name(name) => {
                if let Some((_, value)) = env.iter().find(|(n, _)| *n == name) {
                    value.clone()
                } else {
                    Err(format!("{}", name))?
                }
            }
            Abstract(name, body) => Closure(name, body, env.clone()),
            Apply(box fun, box arg) => {
                let fun = fun.evaluate(env, everything)?;
                let arg = arg.evaluate(env, everything)?;
                match fun {
                    Set(set) => {
                        if set.is_empty() || !set.iter().next().unwrap().is_empty() {
                            let mut result = BTreeSet::new();
                            for tuple in set {
                                if tuple[0] == arg {
                                    result.insert(tuple[1..].to_vec());
                                }
                            }
                            Set(result)
                        } else {
                            Err(format!("{} {}", Set(set), arg))?
                        }
                    }
                    Closure(name, body, closure_env) => {
                        let mut new_env = vec![(name, arg)];
                        new_env.extend_from_slice(&closure_env);
                        body.clone().evaluate(&new_env, everything)?
                    }
                    fun => Err(format!("{} {}", fun, arg))?,
                }
            }
            Solve(def) => unimplemented!(),

            // sugar
            Let(_, _, _) => unimplemented!(),
        })
    }
}

pub fn parse(code: &str) -> Result<Expression, String> {
    syntax::Expression0Parser::new()
        .parse(code)
        .map_err(|error| format!("{:?}", error))
}

pub fn run(code: &str) -> Result<Value, String> {
    parse(code)?.evaluate(
        &vec![],
        &Some(vec![Value::Number(0), Value::Number(1), Value::Number(2)]),
    )
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     macro_rules! assert_eq_run {
//         ( $code1:expr, $($code2:expr),* $(,)? ) => {{
//             $(
//                 println!("{} = {}, {} = {}", $code1, $code2, run($code1), run($code2));
//                 assert_eq!(run($code1), run($code2), "{:?} = {:?}", $code1, $code2);
//             )+
//         }};
//     }

//     #[test]
//     fn basic() {
//         assert_eq!(
//             run("1"),
//             Expression::Materialized(Relation::from_iter(vec![vec![Value::Number(1)]]))
//         );
//         assert_eq!(
//             run(r#""foo""#),
//             Expression::Materialized(Relation::from_iter(vec![vec![Value::String(
//                 "foo".to_owned()
//             )]]))
//         );

//         assert_eq_run!("()", "something");
//         assert_eq_run!("(nothing,)", "nothing");
//         assert_eq_run!("(nothing,something)", "nothing");
//         assert_eq_run!("(something,something)", "something");
//         assert_eq_run!("(1,(2,3))", "((1,2),3)", "(1,2,3)");

//         assert_eq_run!("!nothing", "something");
//         assert_eq_run!("!something", "nothing");
//         assert_eq_run!("!3", "nothing");

//         assert_eq_run!("nothing | something", "something");
//         assert_eq_run!("nothing & something", "nothing");
//         assert_eq_run!("(1 | 2) & (2 | 3)", "2");
//         assert_eq_run!("1 | (2 | 3)", "(1 | 2) | 3");

//         assert_eq_run!("1 = 1", "something");
//         assert_eq_run!("1 = 2", "nothing");
//         assert_eq_run!("(1 | 2) = (2 | 1)", "something");

//         assert_eq_run!("1[1]", "something");
//         assert_eq_run!("1[2]", "nothing");
//         assert_eq_run!(r#"((1, "one") | (2, "two"))[2]"#, r#""two""#);

//         // assert!(run("(1, _, 3)"))
//         assert_eq_run!("(1, 2, _, 3, 4)", "((1, 2), _, 3, 4)");

//         //         run("
//         // let fixpoint_loop = f => old => new => if old = new then new else fixpoint_loop{f, new, f{old}} in
//         // let fixpoint = f => old => fixpoint_loop{f, new, f{old}} in
//         // fixpoint{x => x, 1}
//         // ");
//     }
// }
