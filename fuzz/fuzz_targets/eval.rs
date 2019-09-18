#![no_main]
#[macro_use]
extern crate libfuzzer_sys;

use imp_tests::*;

fuzz_target!(|data: &[u8]| {
    // this comment needs to be here or the macro doesn't work :(
    if let Ok(code) = std::str::from_utf8(data) {
        fuzz_eval(code);
    }
});
