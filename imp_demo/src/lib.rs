#![feature(box_syntax)]
#![feature(result_map_or_else)]

use log::{error, info, warn};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_console_logger::DEFAULT_LOGGER;
use web_sys::HtmlElement;
use web_sys::HtmlTextAreaElement;

#[allow(unused_macros)]
macro_rules! dbg {
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
        Err(error) => outputs.push(format!("Error: {}", error)),
        Ok(expr) => {
            let expr = expr.with_natives(&imp_language::Native::stdlib());
            outputs.push(format!("Parsed: {}", expr));
            match imp_language::eval(expr) {
                Err(error) => outputs.push(format!("Error: {}", error)),
                Ok(value) => outputs.push(format!("Evalled: {}", value)),
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
