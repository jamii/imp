#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(backtrace)]

#[macro_use]
mod macros;
mod analysis;
mod bir;
mod denotation;
mod dir;
mod expression;
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
    debug_info.push(format!("with_natives: {}", expr));

    // let mut type_cache = Cache::new();
    // let _typ = expr
    //     .typecheck(&Environment::new(), &mut type_cache)
    //     .map_err(|e| format!("Type error: {}", e))?;
    // match expr.as_bir(&mut BirContext {
    //     renames: vec![],
    //     type_cache: &type_cache,
    //     gensym: &gensym,
    // }) {
    //     Ok(bir) => {
    //         debug_info.push(format!("bir: {}", d!(&bir)));

    //         let dnf = bir.dnf();
    //         debug_info.push(format!("dnf: {}", d!(&dnf)));
    //     }
    //     Err(err) => debug_info.push(format!("bir err: {}", err)),
    // }

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    expr.funify(&mut type_cache, &gensym);
    log::debug!("funify: {}", expr);
    debug_info.push(format!("funify: {}", &expr));

    // expr = expr.simplify(&HashSet::new());

    // let mut type_cache = Cache::new();
    // let _typ = expr
    //     .typecheck(&Environment::new(), &mut type_cache)
    //     .map_err(|e| format!("Type error: {}", e))?;
    // let dirs = DirsContext::new(&type_cache, &gensym);
    // let result = expr.into_dirs(0, &[], &dirs);
    // debug_info.push(format!("dirs: {:?}\n{:#?}", result, dirs.dirs));

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
    debug_info.push(format!("with_natives: {}", expr));

    let expr = expr.with_unique_names()?;
    log::debug!("with_unique: {}", expr);
    debug_info.push(format!("with_unique: {}", expr));

    let mut expr = expr.desugar(&gensym);
    log::debug!("desugared: {}", expr);
    debug_info.push(format!("desugared: {}", expr));

    let mut type_cache = Cache::new();
    let typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    expr.funify(&mut type_cache, &gensym);
    log::debug!("funify: {}", expr);
    debug_info.push(format!("funify: {}", expr));

    let mut type_cache = Cache::new();
    let _typ = expr
        .typecheck(&Environment::new(), &mut type_cache)
        .map_err(|e| format!("Type error: {}", e))?;
    let dirs = DirsContext::new(&type_cache, &gensym);
    let slot = expr.into_dirs(0, &[], &dirs)?;
    let dirs = dirs.finish();

    let set = dirs.eval(slot)?;

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
