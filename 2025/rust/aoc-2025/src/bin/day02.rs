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

fn is_invalid_id_part1(id: &i64) -> bool {
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

fn compute_invalid_ids<F>(line: &str, is_invalid_id: F) -> Vec<i64>
where
    F: Fn(&i64) -> bool,
{
    let id_ranges: Vec<RangeInclusive<i64>> = line.split(',').map(|s| to_range(s)).collect();

    let invalid_ids: Vec<i64> = id_ranges
        .into_iter()
        .flat_map(|r| r.into_iter().filter(|id| is_invalid_id(id)))
        .collect();

    invalid_ids
}

fn is_invalid_id_part2(id: &i64) -> bool {
    let digits: Vec<i32> = num_to_digits(id);

    for i in 1..=digits.len() / 2 {
        let pattern = &digits[0..i];
        if digits.len() % pattern.len() == 0 {
            let illegal = pattern.repeat(digits.len() / pattern.len());
            if illegal == digits {
                return true;
            }
        }
    }

    false
}

fn part_a() -> i64 {
    let line = read_line("input/day02.txt");

    let invalid_ids = compute_invalid_ids(&line, is_invalid_id_part1);

    // println!("{:?}", invalid_ids);

    invalid_ids.iter().sum()
}

fn part_b() -> i64 {
    let line = read_line("input/day02.txt");

    let invalid_ids = compute_invalid_ids(&line, is_invalid_id_part2);

    invalid_ids.iter().sum()
}
