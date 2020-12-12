use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

#[derive(Debug)]
enum Command {
    North,
    East,
    West,
    South,
    Forward,
    Right,      // Clockwise
    Left,       // Anti-clockwise
}

struct Instruction{
    cmd: Command,
    nb: u32
}

struct Waypoint{
    north: i32,
    east: i32,
}

impl Waypoint{
    fn turn_right(&mut self, nb: u16){
        let nrot : u8 = (nb/90) as u8;
        for _i in 0..nrot {
            let (n, e) = (self.east, self.north);
            self.north = -n;
            self.east = e;
        }
    }

    fn turn_left(&mut self, nb: u16){
        let nrot : u8 = (nb/90) as u8;
        // Swap, then reverse sign of east
        for _i in 0..nrot {
            let (n, e) = (self.east, self.north);
            self.north = n;
            self.east = -e;
        }
    }
}

struct Boat{
    north: i32,
    east: i32,
    waypoint: Waypoint,
}

impl Boat {
    fn compute_manhattan(&self) -> u32 {
        (self.north.abs() + self.east.abs()) as u32
    }

    fn forward_direction(&mut self, nb: u32){
        self.north += (nb as i32)*self.waypoint.north;
        self.east += (nb as i32)*self.waypoint.east;
    }

    fn print_status(&self){
        println!("{}:{} -> {}:{} -> {}", self.waypoint.north, self.waypoint.east, self.north, self.east, self.compute_manhattan());
    }

    fn exec_instruction(&mut self, instruct: &Instruction) {
        match instruct.cmd {
            Command::North => (self.waypoint.north += instruct.nb as i32),
            Command::East => (self.waypoint.east += instruct.nb as i32),
            Command::South => (self.waypoint.north -= instruct.nb as i32),
            Command::West => (self.waypoint.east -= instruct.nb as i32),
            Command::Forward => self.forward_direction(instruct.nb),
            Command::Left => self.waypoint.turn_left(instruct.nb as u16),
            Command::Right => self.waypoint.turn_right(instruct.nb as u16),
        }
    }


}

fn parse_instruction(instruction: &str) -> (String, u32) {
    let cmd: String = instruction[0..1].to_string();
    let nb: u32 = instruction[1..].parse::<u32>().unwrap();
    (cmd, nb)
}


fn instruction_from_tuple(data: (String, u32)) -> Instruction {
    let instruction = Instruction{
        cmd: match data.0.as_str() {
            "N" => Command::North,
            "S" => Command::South,
            "E" => Command::East,
            "W" => Command::West,
            "F" => Command::Forward,
            "R" => Command::Right,
            "L" => Command::Left,
            _ => panic!("Command not recognized {}", data.0),
        },
        nb: data.1,
    };

    instruction
}

fn get_instructions_list(fname: impl AsRef<Path>) -> Vec<Instruction> {
    println!("Getting instructions from file");
    let mut instructions: Vec<Instruction> = Vec::new();

    let file = File::open(fname).expect("no such file");
    let buf = BufReader::new(file);
    let flines : Vec<String> = buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    for instruct in &flines {
        instructions.push(instruction_from_tuple(parse_instruction(instruct)));
    }

    instructions
}

fn start(fname: &str) {
  let mut boat = Boat{north:0, east:0, waypoint: Waypoint{east:10, north:1}};
    let instructions = get_instructions_list(fname);
    for instruct in &instructions {
        boat.exec_instruction(instruct);
        print!("{:?} {}: ", instruct.cmd, instruct.nb);
        boat.print_status();
    }
    println!("Result: {}", boat.compute_manhattan());
}

fn help_msg(){
    println!("Usage: day12 <fname>");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => start(args[1].as_str()),
        _ => help_msg(),
    };
}
