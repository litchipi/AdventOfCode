pub type Allergen = String;
pub type Ingredient = String;

use crate::input::ParsedInput;

pub fn solve(input: ParsedInput){
    let mut safe_matrix = vec![vec![true; input.all_ingr.len()]; input.all_allergen.len()];
    for food in input.food.iter(){
        for allergen in food.1.iter(){
            for ingr in 0..input.all_ingr.len(){
                safe_matrix[*allergen][ingr] &= food.0.contains(&ingr);
            }
        }
    }

    let safe_ingr = (0..input.all_ingr.len())
        .filter(|&el| safe_matrix.iter().all(|line| !line[el]))
        .collect::<Vec<usize>>();

    let occurences = safe_ingr.iter()
        .map(|&ingr| input.food.iter().map(
                |f| f.0.contains(&ingr) as usize
                ).sum::<usize>()
        )
        .sum::<usize>();

    println!("{}", occurences);
}
