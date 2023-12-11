const TEST_INPUT_PART1: &str = include_str!("./test_input_part1");
const TEST_INPUT_PART2: &str = include_str!("./test_input_part2");
const REAL_INPUT: &str = include_str!("./input");

const SPELLED_NUMBERS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

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
        if line.is_empty() {
            continue;
        }

        let f: u32 = {
            let mut result = None;
            'substr: for n in 0..line.len() {
                let substr = line.split_at(n).1;
                assert!(!substr.is_empty());

                // Check for digit
                let first_char = substr.chars().nth(0).unwrap();
                if first_char.is_digit(10) {
                    result = Some(first_char.to_digit(10).unwrap());
                    break 'substr;
                }

                // Check for spelled number
                for (n, spelled) in SPELLED_NUMBERS.iter().enumerate() {
                    if substr.starts_with(spelled) {
                        result = Some((n + 1) as u32);
                        break 'substr;
                    }
                }
            }
            result.unwrap()
        };

        let l = {
            let mut result = None;
            'substr: for n in 0..line.len() {
                let substr = line.split_at(line.len() - n).1;
                if substr.is_empty() {
                    continue;
                }

                // Check for digit
                let first_char = substr.chars().nth(0).unwrap();
                if first_char.is_digit(10) {
                    result = Some(first_char.to_digit(10).unwrap());
                    break 'substr;
                }

                // Check for spelled number
                for (n, spelled) in SPELLED_NUMBERS.iter().enumerate() {
                    if substr.starts_with(spelled) {
                        result = Some((n + 1) as u32);
                        break 'substr;
                    }
                }
            }
            result
        }.unwrap_or(f);
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
    assert_eq!(solve_part2(TEST_INPUT_PART2), 281);
}
