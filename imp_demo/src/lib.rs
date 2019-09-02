#![feature(box_syntax)]
#![feature(result_map_or_else)]

use log::{error, info, warn};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_console_logger::DEFAULT_LOGGER;
use web_sys::HtmlElement;
use web_sys::HtmlTextAreaElement;

#[allow(unused_macros)]
macro_rules! d {
    ($e:expr) => {{
        let val = $e;
        web_sys::console::log_1(&format!("{} = {:#?}", stringify!($e), val).into());
        val
    }};
}

fn update(node: &HtmlElement) {
    let code = node
        .first_element_child()
        .unwrap()
        .dyn_into::<HtmlTextAreaElement>()
        .unwrap()
        .value();
    let mut outputs = vec![];
    match imp_language::parse(&code) {
        Err(error) => {
            outputs.push(format!("Error: {:?}", dbg!(&error)));
        }
        Ok(expr) => {
            let expr = expr.with_natives(&imp_language::Native::stdlib());
            outputs.push(format!("Parsed: {}", dbg!(&expr)));
            let type_env = imp_language::Environment::new();
            let mut type_cache = imp_language::Cache::new();
            let typ = expr.typecheck(&type_env, &mut type_cache);
            outputs.push(format!("Type: {:?}", dbg!(&typ)));
            let scalar_env = imp_language::Environment::new();
            let mut scalar_cache = imp_language::Cache::new();
            expr.scalar(&scalar_env, &mut scalar_cache).unwrap();
            // let lowered = expr.lower(&scalar_cache, &type_cache);
            // outputs.push(format!(
            //     "Lowered: {}",
            //     match dbg!(lowered.map(|lowered| lowered
            //         .into_iter()
            //         .map(|(name, args, body)| format!(
            //             "{} = \\ {} -> {}",
            //             name,
            //             args.join(" "),
            //             body
            //         ))
            //         .collect::<Vec<_>>()
            //         .join("\n")))
            //     {
            //         Ok(s) => format!("Ok:\n {}", s),
            //         Err(s) => format!("Err: {}", s),
            //     }
            // ));
            match imp_language::eval(expr) {
                Err(error) => outputs.push(format!("Error: {}", dbg!(&error))),
                Ok(value) => outputs.push(format!("Evalled: {}", dbg!(&value))),
            }
        }
    }
    node.last_element_child()
        .unwrap()
        .dyn_into::<HtmlElement>()
        .unwrap()
        .set_inner_text(&outputs.join("\r\n\r\n"));
}

#[wasm_bindgen(start)]
pub fn init() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    log::set_logger(&DEFAULT_LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Info);

    let document = web_sys::window().unwrap().document().unwrap();

    let nodes = document.get_elements_by_class_name("imp");
    for i in 0..nodes.length() {
        let node = nodes
            .get_with_index(i)
            .unwrap()
            .dyn_into::<HtmlElement>()
            .unwrap();
        update(&node);
        let closure = Closure::wrap(box {
            let node = node.clone();
            move || update(&node)
        } as Box<dyn Fn()>);
        node.set_onkeyup(Some(closure.as_ref().dyn_ref().unwrap()));
        closure.forget();
    }

    Ok(())
}
