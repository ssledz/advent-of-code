use aoc_2025::read_lines;

// 565 too low
fn main() {
    println!("Solution part_a: {}", part_a());
    println!("Solution part_b: {}", part_b());
}

fn part_a() -> String {
    let lines = read_lines("input/day01.txt");
    let mut acc = 50;
    let mut cnt = 0;
    for line in lines {
        let (l, n) = line.split_at(1);
        let sig = if l == "L" { -1 } else { 1 };
        let n = sig * n.parse::<i32>().unwrap();
        acc = (acc + n) % 100;

        if acc == 0 {
            cnt += 1;
        }

        // println!("line: {}", line);
    }
    cnt.to_string()
}

fn part_b() -> String {
    String::from("Part A result")
}
