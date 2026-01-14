use aoc_2025::num_to_digits;
use aoc_2025::read_line;
use std::ops::RangeInclusive;

fn main() {
    println!("Solution part_a: {}", part_a());
    println!("Solution part_b: {}", part_b());
}

fn to_range(s: &str) -> RangeInclusive<i64> {
    let parts: Vec<&str> = s.split('-').collect();
    let start = parts[0].parse().unwrap();
    let end = parts[1].parse().unwrap();
    start..=end
}

fn is_invalid_id(id: &i64) -> bool {
    let digits: Vec<i32> = num_to_digits(id);
    if digits[0] == 0 {
        return true;
    }

    let len = digits.len();

    if len % 2 == 0 {
        let left: Vec<&i32> = digits.iter().take(len / 2).collect();
        let right: Vec<&i32> = digits.iter().skip(len / 2).collect();
        return left == right;
    }

    false
}

fn part_a() -> i64 {
    let line = read_line("input/day02.txt");
    let id_ranges: Vec<RangeInclusive<i64>> = line.split(',').map(|s| to_range(s)).collect();

    let invalid_ids: Vec<i64> = id_ranges
        .into_iter()
        .flat_map(|r| r.into_iter().filter(|id| is_invalid_id(id)))
        .collect();

    // println!("{:?}", invalid_ids);

    invalid_ids.iter().sum()
}

fn part_b() -> i32 {
    let line = read_line("input/day02.txt");
    -1
}
