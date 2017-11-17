#![feature(proc_macro)]
#![feature(conservative_impl_trait)]
#![feature(slice_patterns)]
#![feature(test)]
#![feature(box_syntax)]

extern crate test;
#[macro_use]
extern crate log;
extern crate env_logger;

extern crate websocket;
// #[macro_use(json, json_internal)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate bincode;
#[macro_use]
extern crate nom;
extern crate csv;

extern crate timely;
extern crate differential_dataflow;
extern crate abomonation;

#[macro_use]
pub mod util;
pub mod language;
pub mod data;
pub mod interpreter;
pub mod compiled;
pub mod dd;
