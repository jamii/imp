#![feature(box_syntax)]
#![feature(box_patterns)]

use lalrpop_util::lalrpop_mod;
use std::collections::HashSet;
use std::fmt;
use std::iter::FromIterator;

macro_rules! iflet {
    ( $p:pat = $e:expr ) => {{
        if let $o = $e {
            true
        } else {
            false
        }
    }};
}

lalrpop_mod!(pub syntax);

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    String(String),
    Number(i64),
}

pub type Tuple = Vec<Value>;

pub type Relation = HashSet<Tuple>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Name {
    Equals,
    Union,
    Intersect,
    Negate,
    Tuple,
    Name {
        name: String,
        scope: Option<u64>, // None after parsing, filled in later
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    // literals
    Nothing,
    Something,
    Everything,
    Literal(Value),

    // abstractions
    Reference(Name),
    AbstractFirst(Box<Name>, Box<Expression>),
    AbstractHigher(Box<Name>, Box<Expression>),
    ApplyFirst(Box<Expression>, Box<Expression>),
    ApplyHigher(Box<Expression>, Box<Expression>),

    SyntaxError(String),

    // used for partial evaluation
    Materialized(Relation),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(string) => write!(f, "{:?}", string),
            Value::Number(number) => write!(f, "{:?}", number),
        }
    }
}

fn write_delimited<T, F: Fn(&mut fmt::Formatter, &T) -> fmt::Result>(
    f: &mut fmt::Formatter,
    delimiter: &str,
    things: &[T],
    write: F,
) -> fmt::Result {
    for (i, thing) in things.iter().enumerate() {
        write(f, thing)?;
        if i != things.len() - 1 {
            write!(f, "{}", delimiter)?;
        }
    }
    Ok(())
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Name::*;
        write!(
            f,
            "{}",
            match self {
                Equals => "=",
                Union => "|",
                Intersect => "&",
                Negate => "!",
                Tuple => panic!(),
                Name { name, .. } => name,
            }
        )
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Name::*;
        use Expression::*;
        match self {
            Nothing => write!(f, "nothing"),
            Something => write!(f, "something"),
            Everything => write!(f, "everything"),
            Literal(value) => write!(f, "{}", value),

            Reference(name) => write!(f, "{}", name),
            AbstractFirst(arg, body) => write!(f, "({} -> {})", arg, body),
            AbstractHigher(arg, body) => write!(f, "({} => {})", arg, body),
            ApplyFirst(function, arg) => write!(f, "{}[{}]", function, arg),
            ApplyHigher(box ApplyHigher(box Reference(Equals), e1), e2) => {
                write!(f, "{} = {}", e1, e2)
            }
            ApplyHigher(box ApplyHigher(box Reference(Union), e1), e2) => {
                write!(f, "{} | {}", e1, e2)
            }
            ApplyHigher(box ApplyHigher(box Reference(Intersect), e1), e2) => {
                write!(f, "{} & {}", e1, e2)
            }
            ApplyHigher(box Reference(Negate), e) => write!(f, "!{}", e),
            ApplyHigher(box ApplyHigher(box Reference(Tuple), e1), e2) => {
                write!(f, "({}, {})", e1, e2)
            }

            ApplyHigher(function, arg) => write!(f, "{}{{{}}}", function, arg),

            SyntaxError(_error) => write!(f, "syntax_error"),

            Materialized(relation) => {
                if relation.len() == 0 {
                    write!(f, "nothing")?;
                } else {
                    let mut tuples = relation.iter().collect::<Vec<_>>();
                    tuples.sort();
                    write_delimited(f, " | ", &*tuples, |f, tuple| {
                        if tuple.len() == 1 {
                            write!(f, "{}", &tuple[0])?;
                        } else {
                            write!(f, "(")?;
                            write_delimited(f, ", ", &*tuple, |f, value| write!(f, "{}", value))?;
                            write!(f, ")")?;
                        }
                        Ok(())
                    })?;
                }
                Ok(())
            }
        }
    }
}

impl Expression {
    fn apply_higher(f: Expression, args: Vec<Expression>) -> Self {
        args.into_iter()
            .fold(f, |f, arg| Expression::ApplyHigher(box f, box arg))
    }

    fn apply_first(f: Expression, args: Vec<Expression>) -> Self {
        args.into_iter()
            .fold(f, |f, arg| Expression::ApplyFirst(box f, box arg))
    }

    fn map<F: Fn(Expression) -> Expression>(self, f: F) -> Expression {
        use Expression::*;
        match self {
            Nothing => Nothing,
            Something => Something,
            Everything => Everything,
            Literal(value) => Literal(value),

            Reference(name) => Reference(name),
            AbstractFirst(args, body) => AbstractFirst(args, box f(*body)),
            AbstractHigher(args, body) => AbstractHigher(args, box f(*body)),
            ApplyFirst(function, arg) => ApplyFirst(box f(*function), box f(*arg)),
            ApplyHigher(function, arg) => ApplyHigher(box f(*function), box f(*arg)),

            SyntaxError(error) => SyntaxError(error),

            Materialized(relation) => Materialized(relation),
        }
    }
}

impl Expression {
    fn evaluate_step(self) -> Expression {
        use crate::Name::*;
        use Expression::*;
        match self {
            Nothing => Materialized(Relation::from_iter(vec![])),
            Something => Materialized(Relation::from_iter(vec![vec![]])),
            Literal(value) => Materialized(Relation::from_iter(vec![vec![value.clone()]])),

            ApplyFirst(box Materialized(relation1), box Materialized(relation2)) => Materialized(
                relation1
                    .into_iter()
                    .flat_map(|tuple1| {
                        relation2.iter().cloned().filter_map(move |tuple2| {
                            if (tuple1.len() >= tuple2.len())
                                && (&tuple1[0..tuple2.len()] == &*tuple2)
                            {
                                Some(tuple1[tuple2.len()..tuple1.len()].to_vec())
                            } else {
                                None
                            }
                        })
                    })
                    .collect(),
            ),

            ApplyHigher(
                box ApplyHigher(box Reference(Tuple), box Materialized(relation1)),
                box Materialized(relation2),
            ) => {
                let mut output = Relation::new();
                for tuple1 in &relation1 {
                    for tuple2 in &relation2 {
                        output.insert(
                            tuple1
                                .iter()
                                .cloned()
                                .chain(tuple2.iter().cloned())
                                .collect(),
                        );
                    }
                }
                Materialized(output)
            }
            ApplyHigher(box Reference(Negate), box Materialized(relation)) => {
                if Materialized(relation) == Nothing.evaluate() {
                    Something.evaluate()
                } else {
                    Nothing.evaluate()
                }
            }
            ApplyHigher(
                box ApplyHigher(box Reference(Union), box Materialized(relation1)),
                box Materialized(relation2),
            ) => Materialized(relation1.union(&relation2).cloned().collect()),
            ApplyHigher(
                box ApplyHigher(box Reference(Intersect), box Materialized(relation1)),
                box Materialized(relation2),
            ) => Materialized(relation1.intersection(&relation2).cloned().collect()),
            ApplyHigher(
                box ApplyHigher(box Reference(Equals), box Materialized(relation1)),
                box Materialized(relation2),
            ) => {
                if relation1 == relation2 {
                    Something.evaluate()
                } else {
                    Nothing.evaluate()
                }
            }

            other => other,
        }
    }

    fn evaluate(self) -> Expression {
        self.map(|e| e.evaluate()).evaluate_step()
    }
}

pub fn parse(code: &str) -> Expression {
    match syntax::Expression0Parser::new().parse(code) {
        Ok(expression) => expression,
        Err(error) => Expression::SyntaxError(format!("{:?}", error)),
    }
}

pub fn run(code: &str) -> Expression {
    parse(code).evaluate()
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eq_run {
        ( $code1:expr, $($code2:expr),* $(,)? ) => {{
            $(
                println!("{} = {}, {} = {}", $code1, $code2, run($code1), run($code2));
                assert_eq!(run($code1), run($code2), "{:?} = {:?}", $code1, $code2);
            )+
        }};
    }

    #[test]
    fn basic() {
        assert_eq!(
            run("1"),
            Expression::Materialized(Relation::from_iter(vec![vec![Value::Number(1)]]))
        );
        assert_eq!(
            run(r#""foo""#),
            Expression::Materialized(Relation::from_iter(vec![vec![Value::String(
                "foo".to_owned()
            )]]))
        );

        assert_eq_run!("()", "something");
        assert_eq_run!("(nothing,)", "nothing");
        assert_eq_run!("(nothing,something)", "nothing");
        assert_eq_run!("(something,something)", "something");
        assert_eq_run!("(1,(2,3))", "((1,2),3)", "(1,2,3)");

        assert_eq_run!("!nothing", "something");
        assert_eq_run!("!something", "nothing");
        assert_eq_run!("!3", "nothing");

        assert_eq_run!("nothing | something", "something");
        assert_eq_run!("nothing & something", "nothing");
        assert_eq_run!("(1 | 2) & (2 | 3)", "2");
        assert_eq_run!("1 | (2 | 3)", "(1 | 2) | 3");

        assert_eq_run!("1 = 1", "something");
        assert_eq_run!("1 = 2", "nothing");
        assert_eq_run!("(1 | 2) = (2 | 1)", "something");

        assert_eq_run!("1[1]", "something");
        assert_eq_run!("1[2]", "nothing");
        assert_eq_run!(r#"((1, "one") | (2, "two"))[2]"#, r#""two""#);

        // assert!(run("(1, _, 3)"))
        assert_eq_run!("(1, 2, _, 3, 4)", "((1, 2), _, 3, 4)");
    }
}
