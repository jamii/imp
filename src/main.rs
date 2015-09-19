#![feature(test)]
#![feature(iter_arith)]
#![feature(step_by)]
#![allow(dead_code)]

extern crate rand;
extern crate test;
extern crate time;
extern crate regex;

use regex::Regex;

macro_rules! time {
    ($name:expr, $expr:expr) => {{
        let start = ::time::precise_time_s();
        let result = $expr;
        let end = ::time::precise_time_s();
        println!("{} took {}s", $name, end - start);
        result
    }};
}

macro_rules! assert_set_eq {
    ($left:expr, $right:expr) => {
        assert_eq!(
            $left.collect::<::std::collections::HashSet<_>>(),
            $right.into_iter().collect::<::std::collections::HashSet<_>>()
            )
    }
}

mod runtime;
mod bootstrap;

fn main() {
    let rex = Regex::new(r#"_|\?[:alnum:]*(:[:alnum:]*)?|"[^"]*"|([:digit:]|\.)+"#).unwrap();
    println!("{:?}", &[
        rex.find("has ?cat bar"),
        rex.find("has \"foo\" bar"),
        rex.find("has 1 bar"),
        rex.find("has 1.92 bar"),
        rex.find("has no bar"),
        rex.find("has a bar"),
        ]);
}
