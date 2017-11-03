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
mod util;
mod language;
mod data;
mod interpreter;
mod dd;

use data::*;

fn main() {
    env_logger::init().unwrap();
    let args: Vec<String> = ::std::env::args().into_iter().collect();
    let args: Vec<&str> = args.iter().map(|s| &**s).collect();
    match *args {
        [] => unreachable!(),
        [_] => println!("Commands: bench editor dataflow"),
        [_, "import"] => data::import(),
        [_, "bench"] => bench::bench_all(),
        [_, "editor"] => interpreter::serve_editor(load_chinook()),
        [_, "editor", "chinook"] => interpreter::serve_editor(load_chinook()),
        [_, "editor", "imdb"] => interpreter::serve_editor(load_imdb()),
        [_, "dataflow"] => dd::serve_dataflow(),
        [_, ref other..] => println!("Unknown command: {}", other.join(" ")),
    }
}

// To run benchmarks:
// RUST_BACKTRACE=1 cargo run --release -- bench
mod bench {
    use test::*;
    use super::data::*;
    use super::language::*;
    use super::interpreter::*;
    use std::fs::File;
    use std::io::prelude::*;

    fn bench<F, T>(name: String, mut f: F)
    where
        F: FnMut() -> T,
    {
        let samples = ::test::bench::benchmark(|b: &mut Bencher| b.iter(&mut f));
        println!("{} ... {}", name, ::test::fmt_bench_samples(&samples));
    }

    pub fn bench_code(name: &str, db: &DB) {
        let mut file = File::open(format!("./{}.imp", name)).unwrap();
        let mut code = String::new();
        file.read_to_string(&mut code).unwrap();
        println!("Code is {:?}", code);
        let code_ast = code_ast(&code, 0);
        for (i, block_ast_or_error) in code_ast.blocks.iter().enumerate() {
            match block_ast_or_error {
                &Ok(ref block_ast) => {
                    bench(format!("compile\t{}_{}", name, i), || {
                        plan(block_ast).unwrap()
                    });
                    let block = plan(block_ast).unwrap();
                    let mut prepared = time!("prepare", prepare_block(&block, db).unwrap());
                    bench(format!("run\t{}_{}", name, i), move || {
                        run_block(&block, &mut prepared)
                    });
                }
                &Err(ref error) => {
                    println!("error\t{}_{}\t{}", name, i, error);
                }
            }
        }
    }

    pub fn bench_all() {
        // bench_code("chinook", &load_chinook());
        bench_code("imdb", &load_imdb());
    }
}
