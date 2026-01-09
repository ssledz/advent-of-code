use std::fs;

pub fn read_lines(path: &str) -> Vec<String> {
    read_line(path).lines().map(String::from).collect()
}

pub fn read_line(path: &str) -> String {
    fs::read_to_string(path).expect(&format!("Error reading file {path}"))
}
