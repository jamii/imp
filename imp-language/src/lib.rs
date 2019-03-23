#![feature(box_syntax)]
#![feature(box_patterns)]

use lalrpop_util::lalrpop_mod;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::fmt;

lalrpop_mod!(pub syntax);

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    String(String),
    Number(i64),
}

pub type Tuple = Vec<Value>;

pub type Relation = HashSet<Tuple>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Nothing,
    Something,
    Everything,
    Value(Value),
    Tuple(Vec<Expression>),
    Negation(Box<Expression>),
    Union(Box<Expression>, Box<Expression>),
    Intersection(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    Relation(Relation),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(string) => write!(f, "{:?}", string),
            Value::Number(number) => write!(f, "{:?}", number),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Nothing => write!(f, "nothing"),
            Something => write!(f, "something"),
            Everything => write!(f, "everything"),
            Value(value) => write!(f, "{}", value),
            Tuple(expressions) => {
                write!(f, "(")?;
                for (i, expression) in expressions.iter().enumerate() {
                    write!(f, "{}", expression)?;
                    if i != expressions.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                if expressions.len() > 0 {
                    write!(f, "{}", expressions[expressions.len()-1])?;
                }
                write!(f, ")")
            }
            Negation(expression) => write!(f, "!{}", expression),
            Union(expression1, expression2) => write!(f, "{} | {}", expression1, expression2),
            Intersection(expression1, expression2) => write!(f, "{} & {}", expression1, expression2),
            Equals(expression1, expression2) => write!(f, "{} = {}", expression1, expression2),
            Application(expression1, expression2) => {
                if let box Tuple(_) = expression2 {
                    write!(f, "{}{}", expression1, expression2)
                } else {
                    panic!()
                }
            }
            Relation(relation) => {
                if relation.len() == 0 {
                    write!(f, "nothing")
                } else {
                    let mut tuples = relation.iter().collect::<Vec<_>>();
                    tuples.sort();
                    for (i, tuple) in tuples.iter().enumerate() {
                        write!(f, "(")?;
                        for (j, value) in tuple.iter().enumerate() {
                            write!(f, "{}", value)?;
                            if j != tuple.len() - 1 {
                                write!(f, ", ")?;
                            }
                        }
                        write!(f, ")")?;
                        if i != relation.len() - 1 {
                            write!(f, " | ")?;
                        }
                    }
                    Ok(())
                }
            }
        }
    }
}

impl Expression {
    fn map<F: Fn(Expression) -> Expression>(self, f: F) -> Expression {
        use Expression::*;
        match self {
            Nothing => Nothing,
            Something => Something,
            Everything => Everything,
            Value(value) => Value(value),
            Tuple(expressions) => Tuple(expressions.into_iter().map(f).collect()),
            Negation(expression) => Negation(box f(*expression)),
            Union(expression1, expression2) => Union(box f(*expression1), box f(*expression2)),
            Intersection(expression1, expression2) => Intersection(box f(*expression1), box f(*expression2)),
            Equals(expression1, expression2) => Equals(box f(*expression1), box f(*expression2)),
            Application(expression1, expression2) => Application(box f(*expression1), box f(*expression2)),
            Relation(relation) => Relation(relation),
        }
    }
}

impl Expression {
    
    fn evaluate_step(self) -> Expression {
        use Expression::*;
        match self {
            Nothing => Relation(HashSet::from_iter(vec![])),
            Something => Relation(HashSet::from_iter(vec![vec![]])),
            Value(value) => Relation(HashSet::from_iter(vec![vec![value.clone()]])),
            Tuple(mut expressions) => {
                expressions.reverse();
                let mut outputs = vec![];
                loop {
                    match (expressions.pop(), expressions.pop()) {
                        (Some(Relation(relation1)), Some(Relation(relation2))) => {
                            let mut output = HashSet::new();
                            for tuple1 in &relation1 {
                                for tuple2 in &relation2 {
                                    output.insert(tuple1.iter().cloned().chain(tuple2.iter().cloned()).collect());
                                }
                            }
                            expressions.push(Relation(output));
                        }
                        (Some(expression1), Some(expression2)) => {
                            outputs.push(expression1);
                            expressions.push(expression2);
                        }
                        (Some(expression1), None) => {
                            outputs.push(expression1);
                        }
                        (None, _) => {
                            break;
                        }
                    }
                }
                match outputs.len() {
                    0 => Something.evaluate(),
                    1 => outputs.into_iter().next().unwrap(),
                    _ => Tuple(outputs),
                }
            },
            Negation(box Relation(relation)) => {
                if Relation(relation) == Nothing.evaluate() {
                    Something.evaluate()
                } else {
                    Nothing.evaluate()
                }
            }
            Union(box Relation(relation1), box Relation(relation2)) => {
                Relation(relation1.union(&relation2).cloned().collect())
            }
            Intersection(box Relation(relation1), box Relation(relation2)) => {
                Relation(relation1.intersection(&relation2).cloned().collect())
            }
            Equals(box Relation(relation1), box Relation(relation2)) => {
                if relation1 == relation2 {
                    Something.evaluate()
                } else {
                    Nothing.evaluate()
                }
            }
            Application(box Relation(relation1), box Relation(relation2)) => {
                Relation(relation1.into_iter().flat_map(|tuple1| {
                    relation2.iter().cloned().filter_map(move |tuple2| {
                        if (tuple1.len() >= tuple2.len()) && (&tuple1[0..tuple2.len()] == &*tuple2) {
                            Some(tuple1[tuple2.len()..tuple1.len()].to_vec())
                        } else {
                            None
                        }
                    })
                }).collect())
            }
            other => other,
        }
    }
    
    fn evaluate(self) -> Expression {
        self.map(|e| e.evaluate()).evaluate_step()
    }
}

pub fn parse(code: &str) -> Expression {
    syntax::Expression0Parser::new().parse(code).unwrap()
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
        assert_eq!(run("1"), Expression::Relation(Relation::from_iter(vec![vec![Value::Number(1)]])));
        assert_eq!(run(r#""foo""#), Expression::Relation(Relation::from_iter(vec![vec![Value::String("foo".to_owned())]])));

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

        assert_eq_run!("(1)(1)", "something");
        assert_eq_run!("(1)(2)", "nothing");
        assert_eq_run!(r#"((1, "one") | (2, "two"))(2)"#, r#""two""#);

        assert!(match run("(1, _, 3)") {
            Expression::Tuple(expressions) => {
                match &*expressions {
                    [_, Expression::Everything, _] => true,
                    _ => false,
                }
            }
            _ => false
        });
        assert_eq_run!("(1, 2, _, 3, 4)", "((1, 2), _, (3, 4))");
    }
}
