#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(backtrace)]

#[macro_use]
mod macros;
mod analysis;
mod decorrelation;
mod denotation;
mod expression;
mod pretty;
mod shared;
mod solver;
mod stdlib;
mod syntax;

use crate::shared::*;
pub use crate::shared::{Expression, Native, Scalar, ScalarType, Value, ValueType};

pub fn run(code: &str, debug_info: &mut Vec<String>) -> Result<(ValueType, Value), String> {
    let expr = if code.is_empty() {
        // mild hack
        Expression::None
    } else {
        parse(&code).map_err(|e| format!("Parse error: {:?}", e))?
    };

    let gensym = Gensym::new();

    let mut expr = expr.with_natives(&Native::stdlib());
    debug_info.push(format!("with_natives: {}", d!(&expr)));

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    expr.funify(&mut type_cache, &gensym);
    debug_info.push(format!("funify: {}", d!(&expr)));

    // expr = expr.simplify(&HashSet::new());

    // let mut type_cache = Cache::new();
    // let _typ = expr
    //     .typecheck(&Environment::new(), &mut type_cache)
    //     .map_err(|e| format!("Type error: {}", e))?;
    // let mut scalar_cache = Cache::new();
    // expr.scalar(&Environment::new(), &mut scalar_cache)?;
    // match expr.lower(&ContainsContext {
    //     scalar_cache: &scalar_cache,
    //     type_cache: &type_cache,
    //     gensym: &gensym,
    // }) {
    //     Ok(lowered) => debug_info.push(format!("lower: {}", d!(&lowered))),
    //     Err(err) => debug_info.push(format!("lower err: {}", err)),
    // }

    let value = expr
        .clone()
        .eval(&Environment::new())
        .map_err(|e| format!("Eval error: {}", e))?;

    Ok((typ, value))
}
