#![feature(test)]
#![feature(iter_arith)]
#![feature(step_by)]
#![allow(dead_code)]
#![feature(slice_patterns)]

extern crate rand;
extern crate test;
extern crate time;
extern crate regex;

use std::io::prelude::*;

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
    ($left:expr, $right:expr) => {{
        let mut left = $left.collect::<Vec<_>>();
        left.sort_by(|a,b| a.partial_cmp(b).unwrap()); left.dedup();
        let mut right = $right;
        right.sort_by(|a,b| a.partial_cmp(b).unwrap()); right.dedup();
        assert_eq!(left, right);
    }}
}

mod runtime;
mod bootstrap;

fn run(filenames: &[String]) -> () {
    print!("Loading...");
    let bootstrap_program = bootstrap::load(filenames);
    print!("compiling...");
    let mut runtime_program = bootstrap::compile(&bootstrap_program);
    print!("running...");
    runtime_program.run();
    println!("done!");
    println!("{:#?}", &runtime_program.views);
    println!("{:#?}", &runtime_program.states[runtime_program.states.len() - 1]);
}

fn watch(filenames: &[String]) -> () {
    loop {
        run(filenames);
    }
}

fn main() {
    use std::env;
    let args = env::args().collect::<Vec<String>>();
    match &*args[1] {
        "--run" => run(&args[2..]),
        "--watch" => watch(&args[2..]),
        _ => panic!("Didn't understand this command:\n {:?}", &args[1..]),
    }
}
