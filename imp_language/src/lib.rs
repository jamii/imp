#![feature(box_syntax)]
#![feature(box_patterns)]

#[macro_use]
mod macros;
mod analysis;
mod denotation;
mod expression;
mod pretty;
mod shared;
mod solver;
mod stdlib;
mod syntax;

use crate::shared::*;
pub use crate::shared::{parse, Cache, Environment, Expression, Native, Scalar, Value};

pub fn eval(expr: Expression) -> Result<Value, String> {
    expr.eval(&Environment::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(code: &str) -> Result<Value, String> {
        parse(code)
            .map_err(|error| format!("{:?}", error))?
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
