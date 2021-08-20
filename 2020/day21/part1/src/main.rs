#![allow(warnings)]

mod algo;
mod input;

fn main(){
    algo::solve(input::parse_input());
}
