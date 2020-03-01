#![feature(box_syntax)]
#![feature(box_patterns)]

use imp_language::*;
use std::fs::File;
use std::io::prelude::*;
use walkdir::WalkDir;

fn display_result<T, E>(result: &Result<T, E>) -> String
where
    T: std::fmt::Display,
    E: std::fmt::Display,
{
    match result {
        Ok(ok) => format!("Ok({})", ok),
        Err(err) => format!("Err({})", err),
    }
}

pub fn run_value_tests(path: &str, input: &str) {
    for test in input.split("\n\n") {
        if test.starts_with("#") {
            // comment
            continue;
        }
        let mut parts = test.split("---").peekable();
        let input = parse(parts.next().unwrap()).unwrap();
        let looped_expected = parse(parts.next().unwrap()).unwrap();
        let flat_expected = parts
            .next()
            .map(|s| parse(s).unwrap())
            .unwrap_or(looped_expected.clone());
        let looped_output = eval_looped(input.clone(), &mut vec![]).map(|(_, value)| value);
        let flat_output = eval_flat(input, &mut vec![]).map(|(_, value)| Value::Set(value));
        run_value_test(path, test, "Looped", looped_output, looped_expected);
        run_value_test(path, test, "Flat", flat_output, flat_expected);
    }
}

fn run_value_test(
    path: &str,
    test: &str,
    kind: &str,
    output: Result<Value, String>,
    expected: Expression,
) {
    match &expected {
        // error
        Expression::Name(name) if name == "error" => {
            assert!(
                output.is_err(),
                "{} eval produced wrong output.\nPath: {}\nTest: {}\nExpected: Err(_)\nActual: {}",
                kind,
                path,
                test,
                display_result(&output)
            );
        }
        // error "some error message"
        Expression::Apply(
            box Expression::Name(name),
            box Expression::Scalar(Scalar::String(error)),
        ) if name == "error" => {
            let expected_output = Err(error.clone());
            assert_eq!(
                output,
                expected_output,
                "{} eval produced wrong output.\nPath: {}\nTest: {}\nExpected: {}\nActual: {}",
                kind,
                path,
                test,
                display_result(&expected_output),
                display_result(&output)
            );
        }
        // value
        _ => {
            let expected_output = Ok(eval_looped(expected, &mut vec![]).unwrap().1);
            assert_eq!(
                output,
                expected_output,
                "{} eval produced wrong output.\nPath: {}\nTest: {}\nExpected: {}\nActual: {}",
                kind,
                path,
                test,
                display_result(&expected_output),
                display_result(&output)
            );
        }
    }
}

pub fn fuzz_parse(data: &[u8]) {
    if let Ok(code) = std::str::from_utf8(data) {
        log::debug!(
            "--------------------\n{:?}\n{}\n--------------------",
            code,
            code
        );
        if let Ok(expr) = parse(code) {
            format!("{}", expr);
        }
    }
}

pub fn fuzz_eval(data: &[u8]) {
    if let Some(expr) = build_expr(data) {
        log::debug!("--------------------\n{}\n--------------------", expr);
        let mut debug_info = vec![];
        let result1 = eval_looped(expr.clone(), &mut debug_info);
        let result2 = eval_flat(expr, &mut debug_info);
        if let Ok((typ, Value::Set(set1))) = result1 {
            if !typ.is_function() {
                let (_typ, set2) = result2.unwrap();
                assert_eq!(set1, set2);
            }
        }
    }
}

fn build_expr(data: &[u8]) -> Option<Expression> {
    let mut stack = vec![];
    let mut bytes = data.into_iter().peekable();
    while let Some(byte) = bytes.next() {
        let expr = match byte {
            0 => Expression::None,
            1 => Expression::Some,
            2 if bytes.peek().is_some() => {
                Expression::Scalar(Scalar::Number(*bytes.next().unwrap() as i64))
            }
            3 if bytes.peek().is_some() => {
                Expression::Scalar(Scalar::String(format!("str{}", bytes.next().unwrap())))
            }
            4 if stack.len() >= 2 => {
                Expression::Union(box stack.pop().unwrap(), box stack.pop().unwrap())
            }
            5 if stack.len() >= 2 => {
                Expression::Intersect(box stack.pop().unwrap(), box stack.pop().unwrap())
            }
            6 if stack.len() >= 2 => {
                Expression::Product(box stack.pop().unwrap(), box stack.pop().unwrap())
            }
            7 if stack.len() >= 1 => Expression::Negate(box stack.pop().unwrap()),
            8 if bytes.peek().is_some() => {
                Expression::Name(format!("var{}", bytes.next().unwrap()))
            }
            9 if bytes.peek().is_some() && stack.len() >= 2 => Expression::Let(
                format!("var{}", bytes.next().unwrap()),
                box stack.pop().unwrap(),
                box stack.pop().unwrap(),
            ),
            10 if stack.len() >= 3 => Expression::If(
                box stack.pop().unwrap(),
                box stack.pop().unwrap(),
                box stack.pop().unwrap(),
            ),
            11 if bytes.peek().is_some() && stack.len() >= 1 => Expression::Abstract(
                format!("var{}", bytes.next().unwrap()),
                box stack.pop().unwrap(),
            ),
            12 if stack.len() >= 2 => {
                Expression::Apply(box stack.pop().unwrap(), box stack.pop().unwrap())
            }
            13 if bytes.peek().is_some() => {
                let name = match bytes.next().unwrap() {
                    0 => "add",
                    1 => "subtract",
                    2 => "negative",
                    // 3 => "permute",
                    // 4 => "reduce",
                    // 5 => "pivot",
                    6 => "as_text",
                    7 => "is_function",
                    8 => "less_than",
                    _ => return None,
                };
                Expression::Name(name.to_owned())
            }
            14 if stack.len() > 1 => Expression::Solve(box stack.pop().unwrap()),
            _ => return None,
        };
        stack.push(expr);
    }
    stack.pop()
}

pub fn fuzz_all() {
    for (path, fun) in &[
        ("../fuzz/artifacts/parse/", fuzz_parse as fn(&[u8])),
        ("../fuzz/artifacts/eval/", fuzz_eval as fn(&[u8])),
    ] {
        for entry in WalkDir::new(path) {
            let entry = entry.unwrap();
            dbg!(&entry);
            if entry.path().is_file() && entry.file_name() != ".gitignore" {
                let mut data = vec![];
                File::open(&entry.path())
                    .unwrap()
                    .read_to_end(&mut data)
                    .unwrap();
                (fun)(&data);
            }
        }
    }
}
