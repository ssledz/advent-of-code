use aoc_2025::read_lines;

fn main() {
    println!("Solution part_a: {}", part_a());
    println!("Solution part_b: {}", part_b());
}

fn largest_joltage_a(jr: &Vec<i32>) -> i64 {
    let (idx, a) = jr[..jr.len() - 1]
        .iter()
        .enumerate()
        .max_by(|a, b| {
            let tmp = a.1.cmp(b.1);
            if tmp == std::cmp::Ordering::Equal {
                a.0.cmp(&b.0).reverse()
            } else {
                tmp
            }
        })
        .unwrap();
    let b = jr[idx + 1..].iter().max().unwrap();
    (*a as i64) * 10 + (*b as i64)
}

fn solver<F>(largest_joltage: F) -> i64
where
    F: Fn(&Vec<i32>) -> i64,
{
    let lines = read_lines("input/day03.txt");
    let jrs: Vec<Vec<i32>> = lines
        .into_iter()
        .map(|line| {
            line.chars()
                .into_iter()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect()
        })
        .collect();

    let largest: Vec<i64> = jrs.into_iter().map(|jr| largest_joltage(&jr)).collect();

    largest.into_iter().sum()
}

fn largest_joltage_b(jr: &Vec<i32>) -> i64 {

    fn go(acc: i64, jr: &Vec<i32>, n: usize) -> i64 {
        -1
    }

    go(0, jr, 12)
}

fn part_a() -> i64 {
    solver(largest_joltage_a)
}

fn part_b() -> i64 {
    solver(largest_joltage_b)
}
