#![feature(proc_macro)]
#![feature(conservative_impl_trait)]
#![feature(slice_patterns)]
#![feature(test)]
#![feature(box_syntax)]

extern crate test;
#[macro_use]
extern crate lazy_static;

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
    let args: Vec<String> = ::std::env::args().into_iter().collect();
    match *args {
        [] => unreachable!(),
        [_] => println!("Commands: bench editor dataflow"),
        [_, ref command, _..] => {
            match &**command {
                "bench" => bench::run(),
                "editor" => interpreter::serve_editor(),
                "dataflow" => dd::serve_dataflow(),
                other => println!("Unknown command: {}", other),
            }
        }
    }
}

mod bench {
    use test::*;
    use super::language::*;

    fn bench<F, T>(name: String, f: F)
    where
        F: Fn() -> T,
    {
        let samples = ::test::bench::benchmark(|b: &mut Bencher| b.iter(&f));
        println!("test {} ... {}", name, ::test::fmt_bench_samples(&samples));
    }

    lazy_static! {
        static ref CHINOOK: DB = load_chinook().unwrap();
    }

    pub fn run() {
        let code = include_str!("bench.imp");
        let code_ast = code_ast(code, 0);
        for (i, block_ast_or_error) in code_ast.blocks.iter().enumerate() {
            let block_ast = block_ast_or_error.as_ref().unwrap();
            let block = plan(block_ast).unwrap();
            let db = &*CHINOOK;
            bench(format!("compile {}", i), || plan(block_ast).unwrap());
            bench(format!("run {}", i), || block.run(db));
        }
    }
}
