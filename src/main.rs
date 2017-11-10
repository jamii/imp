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
mod compiled;
mod dd;

use data::*;

// To run the editor server:
// RUST_BACKTRACE=1 RUST_LOG='imp=debug' cargo run --release -- editor imdb

// To run benchmarks:
// RUST_BACKTRACE=1 cargo run --release -- bench

// To profile:
// cargo build --release
// RUST_BACKTRACE=1 valgrind --tool=callgrind --callgrind-out-file=callgrind.out target/release/imp profile

fn main() {
    env_logger::init().unwrap();
    let args: Vec<String> = ::std::env::args().into_iter().collect();
    let args: Vec<&str> = args.iter().map(|s| &**s).collect();
    match *args {
        [] => unreachable!(),
        [_] => println!("Commands: bench editor dataflow"),
        [_, "import"] => data::import(),
        [_, "bench"] => bench::bench_all(),
        [_, "profile"] => profile::profile("imdb", &load_imdb(), 0),
        [_, "editor"] => interpreter::serve_editor(load_chinook()),
        [_, "editor", "chinook"] => interpreter::serve_editor(load_chinook()),
        [_, "editor", "imdb"] => interpreter::serve_editor(load_imdb()),
        [_, "dataflow"] => dd::serve_dataflow(),
        [_, ref other..] => println!("Unknown command: {}", other.join(" ")),
    }
}

mod bench {
    use test::*;
    use super::data::*;
    use super::language::*;
    use super::interpreter::*;
    use super::compiled;
    use std::fs::File;
    use std::io::prelude::*;

    pub fn bench<F, T>(name: String, mut f: F)
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
                    match plan(block_ast) {
                        Ok(block) => {
                            bench(format!("plan\t{}_{}", name, i), || plan(block_ast).unwrap());
                            let block = &block;
                            let mut prepared =
                                &mut time!("prepare", prepare_block(&block, db).unwrap());

                            match (name, i) {
                                ("imdb", 0) => {
                                    println!("{} results", compiled::q1a(prepared).0.len());
                                    bench(format!("compiled\t{}_{}", name, i), || {
                                        compiled::q1a(prepared);
                                    });
                                }
                                ("imdb", 6) => {
                                    println!("{} results", compiled::q2c(prepared).0.len());
                                    bench(format!("compiled\t{}_{}", name, i), || {
                                        compiled::q2c(prepared);
                                    });
                                }
                                _ => (),
                            }

                            bench(format!("interpreted\t{}_{}", name, i), || {
                                run_staged_block(block, prepared)
                            });
                            bench(format!("interpreted\t{}_{}", name, i), || {
                                run_block(block, prepared)
                            });
                        }
                        Err(ref error) => {
                            println!("error\t{}_{}\t{}", name, i, error);
                        }
                    }
                }
                &Err(ref error) => {
                    println!("error\t{}_{}\t{}", name, i, error);
                }
            }
        }
    }

    pub fn bench_polynomial() {
        let db = compiled::polynomial_db();
        let block = plan(&block_ast(compiled::POLYNOMIAL).unwrap()).unwrap();
        let mut prepared = time!("prepare", prepare_block(&block, &db).unwrap());
        bench(format!("baseline\tpolynomial"), || {
            compiled::polynomial_baseline(&prepared);
        });
        bench(format!("compiled\tpolynomial"), || {
            compiled::polynomial(&prepared);
        });
        bench(format!("compiled\tpolynomial_intermediate"), || {
            compiled::polynomial_intermediate(&prepared);
        });
        bench(format!("interpreted\tpolynomial"), || {
            run_block(&block, &mut prepared).unwrap();
        });
        let magic_block = plan(&block_ast(compiled::POLYNOMIAL_MAGIC).unwrap()).unwrap();
        let mut prepared = time!("prepare", prepare_block(&block, &db).unwrap());
        bench(format!("compiled\tpolynomial_boxfn"), || {
            compiled::polynomial_boxfn(&magic_block, &prepared);
        });
        bench(format!("compiled\tpolynomial_fn"), || {
            compiled::polynomial_fn(&magic_block, &prepared);
        });
        bench(format!("interpreted\tpolynomial_magic"), || {
            run_block(&magic_block, &mut prepared).unwrap();
        });
    }

    pub fn bench_all() {
        bench_polynomial();
        // bench_code("chinook", &load_chinook());
        bench_code("imdb", &load_imdb());
    }
}

mod profile {
    use super::language::*;
    use super::interpreter::*;
    use std::fs::File;
    use std::io::prelude::*;

    pub fn profile(name: &str, db: &DB, block_ix: usize) {
        let mut file = File::open(format!("./{}.imp", name)).unwrap();
        let mut code = String::new();
        file.read_to_string(&mut code).unwrap();
        println!("Code is {:?}", code);
        let code_ast = code_ast(&code, 0);
        let block_ast = code_ast.blocks[block_ix].as_ref().unwrap();
        let block = plan(&block_ast).unwrap();
        let mut prepared = time!("prepare", prepare_block(&block, db).unwrap());
        super::bench::bench(format!("run\t{}", name), move || {
            run_block(&block, &mut prepared)
        });
    }
}
