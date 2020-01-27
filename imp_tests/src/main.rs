use imp_tests::*;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    env_logger::init();
    let args = std::env::args().collect::<Vec<_>>();
    let args = args.iter().map(|s| &**s).collect::<Vec<_>>();
    match &*args {
        [_, "fuzz_parse", path] => {
            let mut data = vec![];
            File::open(path).unwrap().read_to_end(&mut data).unwrap();
            fuzz_parse(&data);
        }
        [_, "fuzz_typecheck", path] => {
            let mut data = vec![];
            File::open(path).unwrap().read_to_end(&mut data).unwrap();
            fuzz_typecheck(&data);
        }
        [_, "fuzz_eval", path] => {
            let mut data = vec![];
            File::open(path).unwrap().read_to_end(&mut data).unwrap();
            fuzz_eval(&data);
        }
        [_, "fuzz_bir", path] => {
            let mut data = vec![];
            File::open(path).unwrap().read_to_end(&mut data).unwrap();
            fuzz_bir(&data);
        }
        _ => panic!("Unknown args: {:?}", args),
    }
}
