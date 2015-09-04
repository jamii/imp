#![feature(test)]
#![feature(slice_bytes)]
#![feature(iter_arith)]

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

mod intersect;
mod join;

fn main() {

}