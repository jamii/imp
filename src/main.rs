#![feature(test)]
#![feature(slice_bytes)]
#![feature(iter_arith)]
#![feature(clone_from_slice)]
#![feature(step_by)]
#![feature(hash_default)]

extern crate rand;
extern crate test;
extern crate time;

macro_rules! time {
    ($name:expr, $expr:expr) => {{
        let start = ::time::precise_time_s();
        let result = $expr;
        let end = ::time::precise_time_s();
        println!("{} took {}s", $name, end - start);
        result
    }};
}

mod chunk;
mod relation;
mod query;

fn main() {
}