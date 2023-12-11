const TEST_INPUT_PART1: &str = include_str!("./test_input_part1");
const TEST_INPUT_PART2: &str = include_str!("./test_input_part2");
const REAL_INPUT: &str = include_str!("./input");

pub fn solve() {
    let res = solve_part2(REAL_INPUT);
    println!("=== RESULT ===");
    println!("{res:?}");
}

fn solve_part1(input: &'static str) -> u32 {
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

fn solve_part2(input: &'static str) -> u32 {
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

#[test]
fn test_part1() {
    assert_eq!(solve_part1(TEST_INPUT_PART1), 142);
}

#[test]
fn test_part2() {
    assert_eq!(solve_part2(TEST_INPUT_PART2), 142);
}
