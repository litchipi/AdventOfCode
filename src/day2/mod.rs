const TEST_INPUT_PART1: &str = include_str!("./test_input_part1");
const TEST_INPUT_PART2: &str = include_str!("./test_input_part2");
const REAL_INPUT: &str = include_str!("./input");

const SPELLED_NUMBERS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

pub fn solve() -> u32 {
    return solve_part2(REAL_INPUT);
}

fn solve_part1(input: &'static str) -> u32 {
    const NUM_RED : u32 = 12;
    const NUM_GREEN : u32 = 13;
    const NUM_BLUE : u32 = 14;

    let mut res = 0;
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let num_game: u32 = line.split_whitespace().nth(1).unwrap().strip_suffix(":").unwrap().parse().unwrap();

        let mut possible = true;
        let games = line.split(":").nth(1).unwrap().split(";");

        'games: for game in games {
            let mut nb_each = (0, 0, 0);
            let cubes = game.split(",");
            for cube in cubes {
                let mut data = cube.split_whitespace();
                let nb: u32 = data.next().unwrap().parse().unwrap();
                let col = data.next().unwrap();
                if col == "red" {
                    nb_each.0 += nb;
                } else if col == "green" {
                    nb_each.1 += nb;
                } else if col == "blue" {
                    nb_each.2 += nb;
                } else {
                    unreachable!();
                }
            }

            possible &= nb_each.0 <= NUM_RED;
            possible &= nb_each.1 <= NUM_GREEN;
            possible &= nb_each.2 <= NUM_BLUE;
            if !possible {
                break 'games;
            }
        }

        if possible {
            res += num_game;
        }
    }
    res
}

fn solve_part2(input: &'static str) -> u32 {
    const NUM_RED : u32 = 12;
    const NUM_GREEN : u32 = 13;
    const NUM_BLUE : u32 = 14;

    let mut res = 0;
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let games = line.split(":").nth(1).unwrap().split(";");

        let mut fewest = (0, 0, 0);
        for game in games {
            let mut nb_each = (0, 0, 0);
            let cubes = game.split(",");
            for cube in cubes {
                let mut data = cube.split_whitespace();
                let nb: u32 = data.next().unwrap().parse().unwrap();
                let col = data.next().unwrap();
                if col == "red" {
                    nb_each.0 += nb;
                } else if col == "green" {
                    nb_each.1 += nb;
                } else if col == "blue" {
                    nb_each.2 += nb;
                } else {
                    unreachable!();
                }
            }

            fewest.0 = fewest.0.max(nb_each.0);
            fewest.1 = fewest.1.max(nb_each.1);
            fewest.2 = fewest.2.max(nb_each.2);
        }

        res += fewest.0 * fewest.1 * fewest.2;
    }
    res
}

#[test]
fn test_part1() {
    assert_eq!(solve_part1(TEST_INPUT_PART1), 8);
}

#[test]
fn test_part2() {
    assert_eq!(solve_part2(TEST_INPUT_PART2), 2286);
}
