#![allow(dead_code, unused_variables)]
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

use std::cmp::{max, min};
 
fn gcd(a: usize, b: usize) -> usize {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y => y,
        ((0, x), _) | ((x, 0), _) => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1)) => {
            let (x, y) = (min(x, y), max(x, y));
            gcd((y - x) >> 1, x)
        }
        _ => unreachable!(),
    }
}
 
fn lcm(a: usize, b: usize) -> usize {
    a * b/gcd(a, b)
}

fn nb_solution(ts: u64, buses : &Vec<(u32, u32)>) -> u32 {
    let mut nres = 0;
    for (bus_id, shift) in buses.iter(){
        if ((ts+(*shift as u64))%(*bus_id as u64)) == 0{
            nres += 1;
        }
    }
    nres
}

fn solve_problem(buses : Vec<(u32, u32)>) -> u64 {
    let bus_maxi_period = buses.iter().enumerate().fold((0, 0), |max, (ind, &val)| if val.0 > max.1 {(ind, val.0)} else {max});
    let increment : u64 = bus_maxi_period.1 as u64;
    let mut ts : u64 = increment - (buses[bus_maxi_period.0].1 as u64);
    println!("Start ts: {}", ts);
    loop{
        ts += increment;
        if nb_solution(ts, &buses) > max_solutions_found{
            increment = lcm(increment,
        if is_solution(ts, &buses){
            break ts;
        }
    }
}

fn get_input(fname: impl AsRef<Path>) -> Vec<(u32, u32)> {
    let file = File::open(fname).expect("no such file");
    let buf = BufReader::new(file);
    let flines : Vec<String> = buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let mut buses : Vec<(u32, u32)> = [].to_vec();
    let mut t = 0;
    for bus in flines[1].split(",") {
        if bus != "x"{
            println!("Bus {} departs at t+{}", bus, t);
            buses.push((bus.parse().unwrap(), t));
        }
        t += 1;
    }

    buses
}

fn start(fname: &str) {
    let buses = get_input(fname);
    let result = solve_problem(buses);
    println!("Result: {}", result);
}

fn help_msg(){
    println!("Usage: day13 <fname>");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => start(args[1].as_str()),
        _ => help_msg(),
    };
}
