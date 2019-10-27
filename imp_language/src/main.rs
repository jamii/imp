use imp_language::*;
use std::io::prelude::*;

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let mut debug_info = vec![];
        let result = run(&line, &mut debug_info);
        println!();
        for d in debug_info {
            println!("{}", d);
        }
        match result {
            Ok((typ, value)) => {
                println!("type = {}", typ);
                println!("{}", value);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
