#![allow(dead_code, unused_variables)]

use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

#[derive(Clone)]
#[derive(Copy)]
struct GridEl{
    is_floor: bool,
    is_occupied: bool,
    next_iter_occupied: bool,
    x: u32,
    y: u32,
}

struct Grid{
    grid: Vec<Vec<GridEl>>,
    seats: Vec<GridEl>,
    width: u32,
    height: u32,
}

impl Grid{
    fn init(&mut self){
        self.width = 0;
        self.height = 0;
        for row in self.grid.iter_mut() {
            for c in row.iter_mut() {
                if self.height == 0{
                    self.width += 1;
                }
                if ! c.is_floor{
                    c.is_occupied = true;
                    c.next_iter_occupied = true;
                    self.seats.push(*c);
                }
            }
            self.height += 1
        }
        println!("Width: {}\nHeight: {}", self.width, self.height);
    }

    fn print_grid(&self){
        for y in 0..self.height{
            for x in 0..self.width{
                let c = self.get_grid_el(x, y);
                if c.is_floor{
                    print!(".");
                }else{
                    if c.is_occupied{
                        print!("#");
                    }else{
                        print!("L");
                    }
                }
            }
            println!("");
        }
        println!("");
    }

    fn get_grid_el(&self, x: u32, y: u32) -> GridEl{
        *self.grid.get(y as usize)
            .expect("Bad Y value")
            .get(x as usize)
            .expect("Bad X value")
    }

    fn set_grid_el(&mut self, seat: GridEl){
        self.grid[seat.y as usize][seat.x as usize] = seat.clone();
    }

    fn count_adjacent_occupied(&mut self, seat: GridEl) -> u32 {
        let mut count : u32 = 0;
        for diff in [(0, 1), (1, 0), (1, 1), (0, -1), (-1, 0), (-1, -1), (1, -1), (-1, 1)].iter() {
            if ((seat.x == 0) & ((diff.0 as i8) < 0)) | ((seat.y == 0)  & ((diff.1 as i8) < 0)){
                continue;
            }
            if ((((seat.x as i32)+diff.0) as u32) >= self.width) | ((((seat.y as i32)+diff.1) as u32) >= self.height){
                continue;
            }
            if self.get_grid_el(((seat.x as i32)+diff.0) as u32, ((seat.y as i32)+diff.1) as u32).is_occupied{
                count += 1;
            }
        }
        count
    }

    fn check_occupied_seat(&mut self, i: usize) -> bool {
        if self.count_adjacent_occupied(self.seats[i]) >= 4 {
            self.seats[i].next_iter_occupied = false;
            self.set_grid_el(self.seats[i]);
            //println!("{} -> {}", self.seats[i].is_occupied, self.seats[i].next_iter_occupied);
            true
        } else {
            false
        }
    }

    fn check_empty_seat(&mut self, i: usize) -> bool {
        if self.count_adjacent_occupied(self.seats[i]) == 0{
            self.seats[i].next_iter_occupied = true;
            self.set_grid_el(self.seats[i]);
            true
        }else{
            false
        }
    }

    fn perform_changes(&mut self){
        for i in 0..self.seats.len(){
     //       print!("{} -> {}", self.seats[i].is_occupied, self.seats[i].next_iter_occupied);
            self.seats[i].is_occupied = self.seats[i].next_iter_occupied;
            self.set_grid_el(self.seats[i]);
      //      println!(" = {} ({})", self.seats[i].is_occupied, self.get_grid_el(self.seats[i].x, self.seats[i].y).is_occupied);
        }
    }

    fn iter(&mut self) -> bool {
        let mut changed : bool = false;
        for i in 0..self.seats.len(){ //seat in self.seats.iter_mut(){
            assert_ne!(self.seats[i].is_floor, true);
            if self.seats[i].is_occupied{
                changed = changed | self.check_occupied_seat(i);
            }else{
                changed = changed | self.check_empty_seat(i);
            }
        }
        self.perform_changes();
        changed
    }

    fn count_occupied_seats(&self) -> u32{
        let mut count : u32 = 0;
        for seat in self.seats.iter() {
            if seat.is_occupied{
                count += 1;
            }
        }
        count
    }
}

fn get_grid_from_file(fname: impl AsRef<Path>) -> Grid{
    let file = File::open(fname).expect("no such file");
    let buf = BufReader::new(file);
    let flines : Vec<String> = buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let width = flines[0].len();
    let height = flines.len();
    let mut seat_grid = vec![vec![GridEl{is_floor:true, is_occupied:false, x:0, y:0, next_iter_occupied:false}; width]; height];
    for (nr, row) in flines.iter().enumerate() {
        for (nc, c) in row.chars().enumerate() {
            seat_grid[nr][nc] = match c {
                'L' => GridEl {is_floor:false, is_occupied:false, next_iter_occupied:false, x:nc as u32, y:nr as u32},
                '.' => GridEl {is_floor:true, is_occupied:false, next_iter_occupied:false, x:nc as u32, y:nr as u32},
                '#' => GridEl {is_floor:false, is_occupied:true, next_iter_occupied:false, x:nc as u32, y:nr as u32},
                _ => panic!("Not supposed to happend")
            }
        }
    }
    Grid{grid:seat_grid, width:0, height:0, seats:[].to_vec()}
}

fn start(fname: &str) {
    let mut grid = get_grid_from_file(fname);
    grid.init();
    grid.print_grid();
    let nbseats = loop {
        let changed = grid.iter();
        grid.print_grid();
        if !changed{
            break grid.count_occupied_seats();
        }
    };
    println!("Nb seats: {}", nbseats);
}

fn help_msg(){
    println!("Usage: day11 <fname>");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => start(args[1].as_str()),
        _ => help_msg(),
    };
}
