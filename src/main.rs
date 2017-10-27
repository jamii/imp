#![feature(proc_macro)]
#![feature(conservative_impl_trait)]
#![feature(slice_patterns)]

extern crate websocket;
// #[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate nom;
extern crate csv;

extern crate timely;
extern crate differential_dataflow;
extern crate abomonation;

mod language;
mod interpreter;
mod dd;

fn main() {
    interpreter::serve_editor();
    dd::serve_dataflow();
}
