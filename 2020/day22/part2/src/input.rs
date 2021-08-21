use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

use crate::algo::{Deck, Card};

#[derive(Debug, StructOpt)]
#[structopt(name = "day22", about = "Day 22 part 1 of the Advent of Code 2021")]
pub struct ArgParser{
    /// Input file
    #[structopt(parse(from_os_str))]
    inputfile: PathBuf,
}

pub struct ParsedInput{
    pub decks: Vec<Deck>
}

impl ParsedInput{
    pub fn new() -> ParsedInput{
        ParsedInput{
            decks: vec![],
        }
    }

    pub fn new_deck(&mut self){
        self.decks.push(vec![]);
    }

    pub fn add_card(&mut self, card: Card){
        self.decks.last_mut().unwrap().push(card);
    }

    pub fn finished(&self){
        assert!(self.decks.len() > 1);
        for deck in 1..self.decks.len(){
            assert_eq!(self.decks[deck].len(), self.decks[0].len());
        }
    }
}

pub fn parse_string(content: String) -> ParsedInput{
    let mut input = ParsedInput::new();

    for line in content.lines(){
        if line.starts_with("Player "){
            input.new_deck();
        } else if line.len() > 0 {
            input.add_card(line.parse::<Card>().unwrap());
        }
    }
    input.finished();
    input
}

pub fn parse_input() -> ParsedInput{
    let inp = ArgParser::from_args();
    let content = fs::read_to_string(inp.inputfile).expect("Cannot read input file");
    parse_string(content)
}
