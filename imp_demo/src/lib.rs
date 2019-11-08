#![deny(non_snake_case)]
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

if is_function result then node "code" {none} {fun_as_text result} else

let rows = sealed_rows result in
let pivoted = sealed_pivot result in
let num_rows = reduce rows rows (a b -> if a < b then b else a) in

let table = node "table" in
let table_row = node "tr" in
let table_cell = node "td" in
let table_header = node "th" in
let code = node "code" {none} in

table {none} {
  (rows (row -> row x table_row {none} {
    (0 x 0 x (table_cell {"style" x "color: lightGrey;"} {code {"("}}))
    |
    ((pivoted row) (col val ->
      (if col = 1 then none else
        (col x 0 x table_cell {"style" x "color: lightGrey;"} {code {" x "}}))
      |
      (col x 1 x table_cell {none} {code {as_text val}})
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

fn render(value: &Value) -> Node {
    match value {
        Value::Closure(..) => Node::tag("code").child(Node::text(&format!("{}", value))),
        Value::Set(set) => {
            let mut table = Node::tag("table");
            for (r, row) in set.iter().enumerate() {
                let mut table_row = Node::tag("tr");
                table_row = table_row.child(
                    Node::tag("td").child(
                        Node::tag("code")
                            .style("color", "lightgray")
                            .child(Node::text("(")),
                    ),
                );
                for (c, scalar) in row.iter().enumerate() {
                    if c != 0 {
                        table_row = table_row.child(
                            Node::tag("td").child(
                                Node::tag("code")
                                    .style("color", "lightgray")
                                    .child(Node::text(" x ")),
                            ),
                        );
                    }
                    table_row =
                        table_row
                            .child(Node::tag("td").child(
                                Node::tag("code").child(Node::text(&format!("{}", scalar))),
                            ));
                }
                table_row =
                    table_row.child(Node::tag("td").style("color", "lightGrey").child(
                        Node::tag("code").child(Node::text(if r == 0 { ")" } else { ") |" })),
                    ));
                table = table.child(table_row);
            }
            table
        }
    }
}

#[wasm_bindgen]
pub fn update(input: &str, output_node: &HtmlElement) {
    output_node.set_inner_html("");
    let tmp = Node::tag("div")
        .attribute("class", "imp-error")
        .child(Node::text("Crashed?"));
    output_node.append_child(&tmp.node).unwrap();

    let mut debug_info = vec![];
    let output = match imp_language::run(&input, &mut debug_info) {
        Ok((typ, output)) => Node::tag("div")
            .child({
                let mut node = Node::tag("div").attribute("class", "imp-debug-info");
                for d in debug_info {
                    node =
                        node.child(Node::tag("div").child(Node::tag("code").child(Node::text(&d))));
                }
                node
            })
            .child(
                Node::tag("div")
                    .attribute("class", "imp-type")
                    .child(Node::tag("code").child(Node::text(&format!("type = {}", typ)))),
            )
            .child(
                Node::tag("div")
                    .attribute("class", "imp-value")
                    .child(render(&output)),
            ),
        Err(error) => Node::tag("div")
            .attribute("class", "imp-error")
            .child(Node::tag("code").child(Node::text(&error))),
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
