use imp_tests::*;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    env_logger::init();
    let args = std::env::args().collect::<Vec<_>>();
    let args = args.iter().map(|s| &**s).collect::<Vec<_>>();
    match args[1] {
        "run_value_tests" => {
            for path in &args[2..] {
                let mut input = String::new();
                println!("Opening {}", path);
                File::open(path)
                    .unwrap()
                    .read_to_string(&mut input)
                    .unwrap();
                run_value_tests(path, &input);
            }
        }
        "fuzz_parse" => {
            for path in &args[2..] {
                let mut data = vec![];
                println!("Opening {}", path);
                File::open(path).unwrap().read_to_end(&mut data).unwrap();
                fuzz_parse(&data);
            }
        }
        "fuzz_eval" => {
            for path in &args[2..] {
                let mut data = vec![];
                println!("Opening {}", path);
                File::open(path).unwrap().read_to_end(&mut data).unwrap();
                fuzz_eval(&data);
            }
        }
        "fuzz_all" => fuzz_all(),
        _ => panic!("Unknown args: {:?}", args),
    }
}
