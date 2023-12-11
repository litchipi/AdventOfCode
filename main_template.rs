#[allow(dead_code)]

const TEST_INPUT: &str = include_str!("../test_input");
const REAL_INPUT: &str = include_str!("../input");

fn solve(input: &'static str) -> u32 {
    let mut res = 0;
    todo!();
    res
}

fn main() {
    let res = solve(REAL_INPUT);
    println!("=== RESULT ===");
    println!("{res:?}");
}

#[test]
fn test_input() {
    assert_eq!(solve(TEST_INPUT), 142);
}
