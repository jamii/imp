use imp_language::*;
use std::fs::File;
use std::io::prelude::*;
use walkdir::WalkDir;

pub fn fuzz_eval(code: &str) {
    if let Ok(expr) = parse(code) {
        if let Ok(result) = eval(expr) {
            format!("{}", result);
        }
    }
}

pub fn fuzz_artifacts() {
    let mut input = String::new();
    for entry in WalkDir::new("../fuzz/artifacts/eval/") {
        let entry = entry.unwrap();
        dbg!(&entry);
        if entry.path().is_file() && entry.file_name() != ".gitignore" {
            input.clear();
            File::open(&entry.path())
                .unwrap()
                .read_to_string(&mut input)
                .unwrap();
            dbg!(&input);
            println!("{}", input);
            fuzz_eval(&input);
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn fuzz_artifacts() {
        super::fuzz_artifacts();
    }
}
