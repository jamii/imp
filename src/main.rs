#![feature(test)]
#![feature(iter_arith)]
#![feature(step_by)]
#![allow(dead_code)]
#![feature(slice_patterns)]

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
    ($left:expr, $right:expr) => {
        assert_eq!(
            $left.collect::<::std::collections::HashSet<_>>(),
            $right.into_iter().collect::<::std::collections::HashSet<_>>()
            )
    }
}

mod runtime;
mod bootstrap;

fn watch(filenames: &[String]) -> () {
    loop {
        print!("Loading...");
        let bootstrap_program = bootstrap::Program::load(filenames);
        print!("compiling...");
        let mut runtime_program = bootstrap_program.compile();
        print!("running...");
        runtime_program.run();
        println!("done!");
        println!("{:#?}", runtime_program.states[runtime_program.states.len() - 1]);
    }
}

fn main() {
    use std::env;
    let args = env::args().collect::<Vec<String>>();
    match &*args[1] {
        "--watch" => watch(&args[2..]),
        _ => panic!("Didn't understand this command:\n {:?}", &args[1..]),
    }
}
