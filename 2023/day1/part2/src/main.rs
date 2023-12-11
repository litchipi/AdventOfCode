#[allow(dead_code)]

const TEST_INPUT: &str = include_str!("../test_input");
const REAL_INPUT: &str = include_str!("../input");

fn solve(input: &'static str) -> u32 {
    let mut res = 0;
    for line in input.lines() {
        if line.is_empty() { continue; }
        let mut nums = line.chars().filter(|c| c.is_numeric()).map(|c| c.to_digit(10).unwrap());
        let f = nums.next().unwrap();
        let l = nums.last().unwrap_or(f);
        println!("{f} {l}");
        res += (f * 10) + l;
    }
    res
}

fn main() {
    let res = solve(REAL_INPUT);
    println!("=== RESULT ===");
    println!("{res:?}");
}

#[test]
fn test_input() {
    assert_eq!(solve(TEST_INPUT), 281);
}
