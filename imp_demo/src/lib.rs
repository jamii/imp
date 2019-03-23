use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn init() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    Ok(())
}

#[wasm_bindgen]
pub fn run(code: &str) -> String {
    format!("{}", imp_language::run(code))
}
