#![feature(box_syntax)]

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
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
    let result = format!(
        "{}\n\n{}\n\n{:#?}",
        &imp_language::parse(&code),
        imp_language::run(&code),
        imp_language::run(&code),
    );
    node.last_element_child()
        .unwrap()
        .dyn_into::<HtmlElement>()
        .unwrap()
        .set_inner_text(&result);
}

#[wasm_bindgen(start)]
pub fn init() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

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