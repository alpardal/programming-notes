fn main() {
    let contents = String::from("this is my file");
    write_to_file("a-file.txt", &contents);
}

fn write_to_file(path: &str, contents: &str) {
    use std::fs::File;
    use std::io::prelude::*;
    let mut f = File::create(path).unwrap();
    f.write_all(contents.as_bytes()).unwrap();
}
