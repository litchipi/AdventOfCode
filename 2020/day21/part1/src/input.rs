use crate::algo::{Ingredient, Allergen};

use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "day21", about = "Day 21 part 1 of the Advent of Code 2021")]
pub struct ArgParser{
    /// Input file
    #[structopt(parse(from_os_str))]
    inputfile: PathBuf,
}

pub struct ParsedInput{
    pub food: Vec<(Vec<Ingredient>, Vec<Allergen>)>,
    pub food_nb: usize,
}

impl ParsedInput{
    pub fn new() -> ParsedInput{
        ParsedInput{
            food: vec![(vec![], vec![])],
            food_nb: 0,
        }
    }

    pub fn new_ingredient(&mut self, ingr: Ingredient){
        self.food.get_mut(self.food_nb).unwrap().0.push(ingr);
    }

    pub fn new_allergen(&mut self, allergen: Allergen){
        self.food.get_mut(self.food_nb).unwrap().1.push(allergen);
    }

    pub fn new_food(&mut self){
        self.food_nb += 1;
        self.food.push((vec![], vec![]));
    }
}

pub fn parse_input() -> ParsedInput{
    let inp = ArgParser::from_args();
    let content = fs::read_to_string(inp.inputfile).expect("Cannot read input file");

    let mut input = ParsedInput::new();
    let mut is_allergen = false;

    for line in content.lines(){
        for element in line.split_whitespace() {
            if element.starts_with("("){
                is_allergen = true;
                continue;
            }
            if is_allergen {
                input.new_allergen(element[..element.len()-1].to_string());
            } else {
                input.new_ingredient(element.to_string());
            }
        }
        input.new_food();
        is_allergen = false;
    }
    input.food.pop();
    input
}
