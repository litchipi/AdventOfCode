#![allow(dead_code, unused_variables)]
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

fn get_input(fname: impl AsRef<Path>) -> (u32, Vec<u32>) {
    let file = File::open(fname).expect("no such file");
    let buf = BufReader::new(file);
    let flines : Vec<String> = buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let timestamp = flines[0].parse().unwrap();
    println!("Timestamp: {}", timestamp);
    let mut buses : Vec<u32> = [].to_vec();
    for bus in flines[1].split(",") {
        if bus != "x"{
            println!("Bus {} found", bus);
            buses.push(bus.parse().unwrap());
        }
    }

    (timestamp, buses)
}

fn compute_next_bus(ts : u32, buses :Vec<u32>) -> (u32, u32){
    let nwait : Vec <u32> = buses.clone().into_iter().map(|n| n-(ts%n)).collect();

    let mut result : (u32, u32) = (u32::MAX, u32::MAX);

    for (i, w) in nwait.into_iter().enumerate() {
        if w < result.0 {
            result = (w, buses[i]);
        }
    }

    (result.0+ts, result.1)
}

fn start(fname: &str) {
    let (timestamp, buses) = get_input(fname);
    let (nextbus, bus_id) = compute_next_bus(timestamp, buses);
    println!("{} {}", nextbus, timestamp);
    assert_eq!(nextbus > timestamp, true);
    let res = (nextbus-timestamp)*bus_id;
    println!("Next bus {} in {} minutes: {}", bus_id, nextbus-timestamp, res);
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
