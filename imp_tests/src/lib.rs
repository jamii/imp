use imp_language::*;

pub fn fuzz_eval(code: &str) {
    if let Ok(expr) = parse(code) {
        if let Ok(result) = eval(expr) {
            format!("{}", result);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use std::io::prelude::*;
    use walkdir::WalkDir;

    #[test]
    fn fuzz_artifacts() {
        let mut input = String::new();
        for entry in WalkDir::new("../fuzz/artifacts/eval/") {
            let entry = entry.unwrap();
            if entry.path().is_file() && entry.file_name() != ".gitignore" {
                input.clear();
                File::open(&entry.path())
                    .unwrap()
                    .read_to_string(&mut input)
                    .unwrap();
                fuzz_eval(&input);
            }
        }
    }
}
