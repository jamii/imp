#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(backtrace)]

#[macro_use]
mod macros;
mod analysis;
mod denotation;
mod expression;
mod flatten;
mod lir;
mod pir;
mod pretty;
mod shared;
mod solver;
mod stdlib;
mod syntax;

use crate::shared::*;
pub use crate::shared::{
    parse, Cache, Environment, Expression, Native, Scalar, ScalarType, Value, ValueType,
};

pub fn eval_looped(
    expr: Expression,
    debug_info: &mut Vec<String>,
) -> Result<(ValueType, Value), String> {
    let gensym = Gensym::new();

    let mut expr = expr.with_natives(&Native::stdlib());
    log::debug!("with_natives: {}", expr);
    // debug_info.push(format!("with_natives: {}", expr));

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    expr.funify(&mut type_cache, &gensym);
    log::debug!("funify: {}", expr);
    // debug_info.push(format!("funify: {}", &expr));

    let value = expr
        .clone()
        .eval(&Environment::new())
        .map_err(|e| format!("Eval error: {}", e))?;

    Ok((typ, value))
}

pub fn eval_flat(
    expr: Expression,
    debug_info: &mut Vec<String>,
) -> Result<(ValueType, Set), String> {
    let gensym = Gensym::new();

    let expr = expr.with_natives(&Native::stdlib());
    log::debug!("with_natives: {}", expr);
    // debug_info.push(format!("with_natives: {}", expr));

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;

    if typ.is_function() {
        return Err(format!("Can't eval, typ is: {}", typ));
    }

    debug!("-------");
    let lirs = expr
        .lirs(&mut type_cache, &gensym)
        .map_err(|err| format!("Lir error: {}", err))?;
    debug!("\nlirs\n");
    for lir in &lirs.lirs {
        debug!("{}", lir);
        lir.validate().unwrap();
    }
    debug!("-------");
    debug_info.push(format!("lir:\n{}", lirs));

    let pirs = lirs.pirs();
    debug!("\npirs\n");
    for (i, pir) in pirs.pirs.iter().enumerate() {
        debug!("{} = {:?}", i, pir);
    }
    debug!("-------");
    debug_info.push(format!("pir:\n{}", pirs));

    let set = pirs.eval()?;
    Ok((typ, set))
}

pub fn run_looped(code: &str, debug_info: &mut Vec<String>) -> Result<(ValueType, Value), String> {
    let expr = if code.is_empty() {
        // mild hack
        Expression::None
    } else {
        parse(&code).map_err(|e| format!("Parse error: {:?}", e))?
    };

    eval_looped(expr, debug_info)
}

pub fn run_flat(code: &str, debug_info: &mut Vec<String>) -> Result<(ValueType, Set), String> {
    let expr = if code.is_empty() {
        // mild hack
        Expression::None
    } else {
        parse(&code).map_err(|e| format!("Parse error: {:?}", e))?
    };

    eval_flat(expr, debug_info)
}
