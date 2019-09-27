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

fn find(class: &str) -> HtmlElement {
    web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_elements_by_class_name(class)
        .get_with_index(0)
        .unwrap()
        .dyn_into::<HtmlElement>()
        .unwrap()
}

#[wasm_bindgen(start)]
pub fn init() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    log::set_logger(&DEFAULT_LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Info);

    for node in &[find("imp-repl"), find("imp-render")] {
        let closure = Closure::wrap(box { move || update() } as Box<dyn Fn()>);
        node.set_onkeyup(Some(closure.as_ref().dyn_ref().unwrap()));
        closure.forget();
    }
    update();

    Ok(())
}

fn update() {
    let result = parse_from(&find("imp-repl"))
        .and_then(|repl_expr| eval_etc(repl_expr, &Environment::new()));
    let typ = parse_from(&find("imp-repl")).and_then(|repl_expr| type_etc(repl_expr));

    let mut nodes = vec![];
    nodes.push(Node::tag("div").child(Node::text(&match typ {
        Ok(typ) => format!("Type: {}", typ),
        Err(error) => format!("Type error: {}", error),
    })));
    nodes.push(match result {
        Ok(result) => {
            let rendered = parse_from(&find("imp-render")).and_then(|render_expr| {
                eval_etc(
                    Expression::Apply(
                        box render_expr,
                        box Expression::Seal(box Expression::Name("rendered".to_owned())),
                    ),
                    &Environment::from(vec![("rendered".to_owned(), result)]),
                )
            });
            match rendered.and_then(Value::unseal) {
                Ok(rendered) => render(&rendered),
                Err(error) => {
                    Node::tag("div").child(Node::text(&format!("From renderer: {}", error)))
                }
            }
        }
        Err(error) => Node::tag("span").child(Node::text(&error)),
    });
    let result_node = find("imp-result").last_element_child().unwrap();
    result_node.set_inner_html("");
    for node in nodes {
        result_node.append_child(&node.node).unwrap();
    }
}

fn parse_from(node: &HtmlElement) -> Result<Expression, String> {
    let code = node
        .first_element_child()
        .unwrap()
        .dyn_into::<HtmlTextAreaElement>()
        .unwrap()
        .value();
    if code.is_empty() {
        // mild hack
        Ok(Expression::Nothing)
    } else {
        parse(&code).map_err(|e| format!("{:?}", e))
    }
}

fn type_etc(expr: Expression) -> Result<ValueType, String> {
    let expr = expr.with_natives(&Native::stdlib());

    let type_env = Environment::new();
    let mut type_cache = Cache::new();
    let typ = expr.typecheck(&type_env, &mut type_cache)?;

    Ok(typ)
}

fn eval_etc(expr: Expression, environment: &Environment<Value>) -> Result<Value, String> {
    let expr = expr.with_natives(&Native::stdlib());

    let value = expr.eval(environment)?;

    Ok(value)
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
                                                    children.push(render(value))
                                                }
                                                Scalar::String(string) => {
                                                    children.push(Node::text(string))
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
    let mut node = Node::tag(tag.unwrap());
    for (key, val) in props.unwrap() {
        node = node.attribute(&key, val);
    }
    for child in children.unwrap() {
        node = node.child(child);
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
