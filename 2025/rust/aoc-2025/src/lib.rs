use std::fs;

pub fn read_lines(path: &str) -> Vec<String> {
    let contents = fs::read_to_string(path).expect(&format!("Error reading file {path}"));
    contents.lines().map(String::from).collect()
}
