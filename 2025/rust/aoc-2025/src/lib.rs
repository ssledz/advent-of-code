use num::Num;
use std::fs;

pub fn read_lines(path: &str) -> Vec<String> {
    read_line(path).lines().map(String::from).collect()
}

pub fn read_line(path: &str) -> String {
    fs::read_to_string(path).expect(&format!("Error reading file {path}"))
}

pub fn num_to_digits<T: Num + ToString>(n: &T) -> Vec<i32> {
    n.to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect()
}
