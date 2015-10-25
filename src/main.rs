#![feature(test)]
#![feature(iter_arith)]
#![feature(step_by)]
#![allow(dead_code)]
#![feature(slice_patterns)]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate rand;
extern crate test;
extern crate time;
extern crate regex;

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

fn watch(filenames: &[String]) -> () {
    loop {
        print!("Loading...");
        let bootstrap_program = bootstrap::load(filenames);
        print!("compiling...");
        let mut runtime_program = bootstrap::compile(&bootstrap_program);
        print!("running...");
        runtime_program.run();
        println!("done!");
        println!("{:#?}", runtime_program.states[runtime_program.states.len() - 1]);
    }
}

fn main() {
    use std::env;
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 3 || args[1] != "--watch" {
        println!("usage: {} --watch <imp-program-files>...", args[0]);
        return;
    }
    watch(&args[2..]);
}
