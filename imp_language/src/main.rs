#![deny(non_snake_case)]

use imp_language::*;
use std::io::prelude::*;

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let mut debug_info = vec![];
        let result1 = run_looped(&line, &mut debug_info);
        let result2 = run_flat(&line, &mut debug_info);
        println!();
        for d in debug_info {
            println!("{}", d);
        }
        match result1 {
            Ok((typ, value)) => {
                println!("type = {}", typ);
                println!("{}", value);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
        match result2 {
            Ok((typ, set)) => {
                println!("type = {}", typ);
                println!("{}", Value::Set(set));
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
