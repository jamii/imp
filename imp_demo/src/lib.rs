#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(result_map_or_else)]

use std::borrow::Borrow;
use std::fmt::Display;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_console_logger::DEFAULT_LOGGER;
use web_sys::Element;
use web_sys::EventTarget;
use web_sys::HtmlElement;
use web_sys::HtmlTextAreaElement;

use imp_language::*;

const RENDERER: &'static str = r#"
result ->

let node = tag props children -> {
  "tag" x tag |
  "props" x props |
  "children" x children
} in

if is_function result then node "pre" {"style" x "margin: 0; padding: 0;"} {fun_as_text result} else

let rows = sealed_rows result in
let pivoted = sealed_pivot result in
let num_rows = reduce rows rows (a b -> if a < b then b else a) in

let table = node "table" in
let table_row = node "tr" in
let table_cell = node "td" in
let table_header = node "th" in
let code = node "code" {nothing} in

table {nothing} {
  (rows (row -> row x table_row {nothing} {
    (0 x 0 x (table_cell {"style" x "color: lightGrey;"} {code {"("}}))
    |
    ((pivoted row) (col val ->
      (if col = 1 then nothing else
        (col x 0 x table_cell {"style" x "color: lightGrey;"} {code {"x"}}))
      |
      (col x 1 x table_cell {nothing} {code {as_text val}})
    ))
    |
    (10000 x 0 x (table_cell {"style" x "color: lightGrey;"} {code {if row = num_rows then ")" else ") |"}}))
  }))
}
"#;

#[allow(unused_macros)]
macro_rules! d {
    ($e:expr) => {{
        let val = $e;
        web_sys::console::log_1(&format!("{} = {:#?}", stringify!($e), val).into());
        val
    }};
}

fn find_first(node: &HtmlElement, class: &str) -> Option<HtmlElement> {
    let nodes = node.get_elements_by_class_name(class);
    if nodes.length() > 0 {
        Some(
            nodes
                .get_with_index(0)
                .unwrap()
                .dyn_into::<HtmlElement>()
                .unwrap(),
        )
    } else {
        None
    }
}

fn find_all(class: &str) -> Vec<HtmlElement> {
    let nodes = web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_elements_by_class_name(class);
    (0..nodes.length())
        .map(|i| {
            nodes
                .get_with_index(i)
                .unwrap()
                .dyn_into::<HtmlElement>()
                .unwrap()
        })
        .collect()
}

fn run(code: &str) -> Result<Value, String> {
    let expr = if code.is_empty() {
        // mild hack
        Expression::Nothing
    } else {
        parse(&code).map_err(|e| format!("Parse error: {:?}", e))?
    };
    Ok(expr
        .with_natives(&Native::stdlib())
        .with_unique_names()?
        .lift_lets()
        .eval(&Environment::new())?)
    // let type_env = Environment::new();
    // let mut type_cache = Cache::new();
    // let typ = expr
    //     .typecheck(&type_env, &mut type_cache)
    //     .map_err(|e| format!("Type error: {}", e))?;
}

fn run_ui(value: Value) -> Result<Value, String> {
    let expr = parse(&RENDERER).map_err(|e| format!("In renderer: Parse error: {:?}", e))?;
    let expr = expr.with_natives(&Native::stdlib()).with_unique_names()?; // .lift_lets();
    Ok(Expression::Apply(
        box expr,
        box Expression::Seal(box Expression::Name("value".to_owned())),
    )
    .eval(&Environment::from(vec![("value".to_owned(), value)]))?)
}

fn render(value: &Value) -> Result<Node, String> {
    let mut tag = None;
    let mut props = None;
    let mut children = None;
    match value {
        Value::Set(set) => {
            for row in set {
                match &row[0] {
                    Scalar::String(string) => match &**string {
                        "tag" => {
                            tag = if let Scalar::String(tag) = &row[1] {
                                Some(tag)
                            } else {
                                None
                            }
                        }
                        "props" => {
                            props = Some({
                                let mut props = vec![];
                                match Scalar::unseal(row[1].clone()) {
                                    Ok(Value::Set(set)) => {
                                        for row in set.into_iter() {
                                            match &*row {
                                                [Scalar::String(key), Scalar::String(val)] => {
                                                    props.push((key.to_owned(), val.to_owned()))
                                                }
                                                _ => {
                                                    return Err(format!(
                                                        "Unexpected value in renderer: {:?}",
                                                        row
                                                    ));
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Unexpected value in renderer: {:?}",
                                            row
                                        ));
                                    }
                                }
                                props
                            });
                        }
                        "children" => {
                            children = Some({
                                let mut children = vec![];
                                match Scalar::unseal(row[1].clone()) {
                                    Ok(Value::Set(set)) => {
                                        let mut rows = set.iter().collect::<Vec<_>>();
                                        rows.sort();
                                        for row in rows {
                                            let scalar = &row[row.len() - 1];
                                            match scalar {
                                                Scalar::Sealed(..) => children.push(render(
                                                    &Scalar::unseal(scalar.clone())?,
                                                )?),
                                                Scalar::String(string) => {
                                                    children.push(Node::text(string))
                                                }
                                                _ => {
                                                    return Err(format!(
                                                        "Unexpected value in renderer: {}",
                                                        scalar
                                                    ));
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Unexpected value in renderer: {:?}",
                                            row
                                        ));
                                    }
                                }
                                children
                            });
                        }
                        _ => return Err(format!("Unexpected value in renderer: {}", string)),
                    },
                    _ => return Err(format!("Unexpected value in renderer: {:?}", row)),
                }
            }
        }
        _ => return Err(format!("Unexpected value in renderer: {}", value)),
    }
    let mut node = Node::tag(tag.unwrap());
    for (key, val) in props.unwrap() {
        node = node.attribute(&key, val);
    }
    for child in children.unwrap() {
        node = node.child(child);
    }
    Ok(node)
}

#[wasm_bindgen]
pub fn update(input: &str, output_node: &HtmlElement) {
    let output = match run(&input)
        .map_err(|e| format!("Eval error: {}", e))
        .and_then(|value| run_ui(value).map_err(|e| format!("In renderer: Eval error: {}", e)))
        .and_then(|ui| render(&Value::unseal(ui)?))
    {
        Ok(output) => output,
        Err(error) => Node::tag("div").child(Node::text(&error)),
    };
    output_node.set_inner_html("");
    output_node.append_child(&output.node).unwrap();
}

#[wasm_bindgen(start)]
pub fn init() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    log::set_logger(&DEFAULT_LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Info);

    // for node in find_all("imp-repl") {
    //     let closure = {
    //         let node = node.clone();
    //         Closure::wrap(box { move || update(&node) } as Box<dyn Fn()>)
    //     };
    //     node.set_onkeyup(Some(closure.as_ref().dyn_ref().unwrap()));
    //     closure.forget();
    //     update(&node);
    // }

    // for node in find_all("imp-status") {
    //     node.set_inner_text("imp loaded succesfully!");
    //     node.style().set_property("color", "green").unwrap();
    // }

    Ok(())
}

// ================================================================================

struct Node {
    node: web_sys::Node,
}

impl Node {
    fn tag(tag_name: &str) -> Self {
        Node {
            node: web_sys::window()
                .unwrap()
                .document()
                .unwrap()
                .create_element(tag_name)
                .unwrap()
                .into(),
        }
    }

    fn text(text: &str) -> Self {
        Node {
            node: web_sys::window()
                .unwrap()
                .document()
                .unwrap()
                .create_text_node(text)
                .into(),
        }
    }

    fn attribute<S>(self, name: &str, value: S) -> Self
    where
        S: Borrow<str>,
    {
        let element: Element = self.node.dyn_into().unwrap();
        element.set_attribute(name, value.borrow()).unwrap();
        Node {
            node: element.into(),
        }
    }

    // TODO find an alternative to display that doesn't require reallocating &str
    fn style<S>(self, name: &str, value: S) -> Self
    where
        S: Display,
    {
        let element: HtmlElement = self.node.dyn_into().unwrap();
        element
            .style()
            .set_property(name, &format!("{}", value))
            .unwrap();
        Node {
            node: element.into(),
        }
    }

    fn child(self, node: Node) -> Self {
        self.node.append_child(&node.node).unwrap();
        self
    }

    fn children<I>(mut self, nodes: I) -> Self
    where
        I: IntoIterator<Item = Node>,
    {
        for node in nodes.into_iter() {
            self = self.child(node);
        }
        self
    }

    fn on<F>(self, event_name: &str, handler: F) -> Self
    where
        F: FnMut() + 'static,
    {
        let boxed = Box::new(handler) as Box<dyn FnMut()>;
        let closure = Closure::wrap(boxed);
        let listener: &js_sys::Function = closure.as_ref().unchecked_ref();
        let event_target: EventTarget = self.node.into();
        event_target
            .add_event_listener_with_callback(event_name, &listener)
            .unwrap();
        // TODO figure out how to manage closure
        std::mem::forget(closure);
        Node {
            node: event_target.dyn_into().unwrap(),
        }
    }
}
