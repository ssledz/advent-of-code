use aoc_2025::read_lines;

fn main() {
    println!("Solution part_a: {}", part_a());
    println!("Solution part_b: {}", part_b());
}

fn part_a() -> i32 {
    let lines = read_lines("input/day01.txt");
    count0s(&lines, |acc, n| {
        *acc = (*acc + n).rem_euclid(100);
        if *acc == 0 { 1 } else { 0 }
    })
}

fn part_b() -> i32 {
    let lines = read_lines("input/day01.txt");
    count0s(&lines, |acc, n| {
        let q = n / 100;
        let r = n - q * 100;
        let acc2 = *acc + r;
        let cnt = if *acc != 0 && acc2 <= 0 || acc2 >= 100 {
            1
        } else {
            0
        };
        *acc = acc2.rem_euclid(100);
        q.abs() + cnt
    })
}

fn count0s<F>(lines: &Vec<String>, mut f: F) -> i32
where
    F: FnMut(&mut i32, i32) -> i32,
{
    let mut acc = 50;
    let mut cnt = 0;
    for line in lines {
        let (l, n) = line.split_at(1);
        let sig = if l == "L" { -1 } else { 1 };
        let n = sig * n.parse::<i32>().unwrap();
        cnt = cnt + f(&mut acc, n);
    }
    cnt
}
