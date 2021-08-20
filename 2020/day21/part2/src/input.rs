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
    pub all_ingr: Vec<Ingredient>,
    pub all_allergen: Vec<Allergen>,

    pub food: Vec<(Vec<usize>, Vec<usize>)>,
    pub food_nb: usize,
}

impl ParsedInput{
    pub fn new() -> ParsedInput{
        ParsedInput{
            all_ingr: vec![],
            all_allergen: vec![],
            food: vec![(vec![], vec![])],
            food_nb: 0,
        }
    }

    pub fn new_ingredient(&mut self, ingr: Ingredient){
        let ingr_index = if !self.all_ingr.contains(&ingr){
            self.all_ingr.push(ingr.clone());
            self.all_ingr.len()-1
        } else {
            self.all_ingr.iter().position(|x| x == &ingr).unwrap()
        };
        self.food.get_mut(self.food_nb).unwrap().0.push(ingr_index);
    }

    pub fn new_allergen(&mut self, allergen: Allergen){
        let allergen_index = if !self.all_allergen.contains(&allergen){
            self.all_allergen.push(allergen.clone());
            self.all_allergen.len()-1
        } else {
            self.all_allergen.iter().position(|x| x == &allergen).unwrap()
        };
        self.food.get_mut(self.food_nb).unwrap().1.push(allergen_index);
    }

    pub fn new_food(&mut self){
        self.food_nb += 1;
        self.food.push((vec![], vec![]));
    }
}

pub fn parse_string(content: String) -> ParsedInput{
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

pub fn parse_input() -> ParsedInput{
    let inp = ArgParser::from_args();
    let content = fs::read_to_string(inp.inputfile).expect("Cannot read input file");
    parse_string(content)
}

#[test]
fn test_parse_string(){
    let inp = parse_string("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)".to_string());

    for ingr in vec!["mxmxvkd", "kfcds", "sqjhc", "nhms", "sbzzf", "fvjkl"].iter(){
        assert!(inp.all_ingr.contains(&ingr.to_string()));
    }

    for allergen in vec!["dairy", "soy", "fish"].iter(){
        assert!(inp.all_allergen.contains(&allergen.to_string()));
    }
}
