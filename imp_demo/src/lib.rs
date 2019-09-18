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
    let mut evalled_node = None;
    match parse(&code) {
        Err(error) => {
            outputs.push(format!("Error: {:?}", dbg!(&error)));
        }
        Ok(expr) => {
            outputs.push(format!("Parsed: {}", dbg!(&expr)));
            let expr = expr.with_natives(&Native::stdlib());
            let type_env = Environment::new();
            let mut type_cache = Cache::new();
            let typ = expr.typecheck(&type_env, &mut type_cache);
            outputs.push(format!("Type: {:?}", dbg!(&typ)));
            let scalar_env = Environment::new();
            let mut scalar_cache = Cache::new();
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
            match eval(expr) {
                Err(error) => outputs.push(format!("Error: {}", dbg!(&error))),
                Ok(value) => {
                    outputs.push(format!("Evalled: {}", dbg!(&value)));
                    outputs.push(format!("Rendered:"));
                    evalled_node = Some(render(&Value::unseal(value).unwrap()));
                }
            }
        }
    }
    node.last_element_child()
        .unwrap()
        .dyn_into::<HtmlElement>()
        .unwrap()
        .set_inner_text(&outputs.join("\r\n\r\n"));
    if let Some(evalled_node) = evalled_node {
        node.last_element_child()
            .unwrap()
            .append_child(&evalled_node.node)
            .unwrap();
    }
}

enum Child {
    Node(Node),
    Text(String),
}

fn render(value: &Value) -> Node {
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
                                match &row[1] {
                                    Scalar::Sealed(box Value::Set(set)) => {
                                        for row in set.into_iter() {
                                            match &**row {
                                                [Scalar::String(key), Scalar::String(val)] => {
                                                    props.push((key.to_owned(), val.to_owned()))
                                                }
                                                _ => panic!("a {:?}", row),
                                            }
                                        }
                                    }
                                    _ => panic!("b {:?}", row),
                                }
                                props
                            });
                        }
                        "children" => {
                            children = Some({
                                let mut children = vec![];
                                match &row[1] {
                                    Scalar::Sealed(box Value::Set(set)) => {
                                        let mut rows = set.iter().collect::<Vec<_>>();
                                        rows.sort();
                                        for row in rows {
                                            let scalar = &row[row.len() - 1].clone();
                                            match scalar {
                                                Scalar::Sealed(box value) => {
                                                    children.push(Child::Node(render(value)))
                                                }
                                                Scalar::String(string) => {
                                                    children.push(Child::Text(string.clone()))
                                                }
                                                _ => panic!("c {}", scalar),
                                            }
                                        }
                                    }
                                    _ => panic!("d {:?}", row),
                                }
                                children
                            });
                        }
                        _ => panic!("e {}", string),
                    },
                    _ => panic!("f {:?}", row),
                }
            }
        }
        _ => panic!("g {}", value),
    }
    d!(value);
    let mut node = Node::tag(tag.unwrap());
    for (key, val) in props.unwrap() {
        node = node.attribute(&key, val);
    }
    for child in children.unwrap() {
        node = match child {
            Child::Node(child_node) => node.child(child_node),
            Child::Text(text) => node.text(&text),
        }
    }
    node
}

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

    // fn text(text: &str) -> Self {
    //     Node {
    //         node: web_sys::window()
    //             .unwrap()
    //             .document()
    //             .unwrap()
    //             .create_text_node(text)
    //             .into(),
    //     }
    // }

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

    fn text(self, text: &str) -> Self {
        self.node
            .append_child(
                &web_sys::window()
                    .unwrap()
                    .document()
                    .unwrap()
                    .create_text_node(text)
                    .into(),
            )
            .unwrap();
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
