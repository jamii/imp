#![feature(box_syntax)]
#![feature(bind_by_move_pattern_guards)]

use imp_language::*;
use std::fs::File;
use std::io::prelude::*;
use walkdir::WalkDir;

pub fn fuzz_parse(data: &[u8]) {
    if let Ok(code) = std::str::from_utf8(data) {
        // dbg!(&code);
        if let Ok(expr) = parse(code) {
            format!("{}", expr);
        }
    }
}

pub fn fuzz_eval(data: &[u8]) {
    if let Some(expr) = build_expr(data) {
        // dbg!(&expr);
        if let Ok(result) = eval(expr) {
            format!("{}", result);
        }
    }
}

pub fn fuzz_typecheck(data: &[u8]) {
    if let Some(expr) = build_expr(data) {
        // dbg!(&expr);
        let type_env = Environment::new();
        let mut type_cache = Cache::new();
        let _typ = expr.typecheck(&type_env, &mut type_cache);
        let scalar_env = Environment::new();
        let mut scalar_cache = Cache::new();
        let _scalar = expr.scalar(&scalar_env, &mut scalar_cache);
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
                    3 => "permute",
                    4 => "reduce",
                    5 => "pivot",
                    6 => "as_text",
                    _ => return None,
                };
                Expression::Name(name.to_owned())
            }
            14 if stack.len() >= 1 => Expression::Seal(box stack.pop().unwrap()),
            15 if stack.len() >= 1 => Expression::Unseal(box stack.pop().unwrap()),
            // TODO solve
            _ => return None,
        };
        stack.push(expr);
    }
    stack.pop()
}

pub fn fuzz_artifacts() {
    for (path, fun) in &[
        ("../fuzz/artifacts/parse/", fuzz_parse as fn(&[u8])),
        ("../fuzz/artifacts/eval/", fuzz_eval as fn(&[u8])),
        ("../fuzz/artifacts/typecheck/", fuzz_typecheck as fn(&[u8])),
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

#[cfg(test)]
mod test {
    #[test]
    fn fuzz_artifacts() {
        super::fuzz_artifacts();
    }
}
