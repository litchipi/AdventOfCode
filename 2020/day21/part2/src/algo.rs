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

    let mut pairs : Vec<(Allergen, Ingredient)> = vec![];
    while pairs.len() < input.all_allergen.len(){
        for ingr in 0..input.all_ingr.len(){
            let allergens = safe_matrix.iter().map(|al| al[ingr]).collect::<Vec<bool>>();

            if allergens.iter().map(|&el| el as usize).sum::<usize>() == 1 {
                let allergen_ind = allergens.iter().position(|&el| el).unwrap();

                pairs.push((
                    input.all_allergen[allergen_ind].clone(),
                    input.all_ingr[ingr].clone()
                ));

                println!("Pair found: {:?}", pairs.last());
                safe_matrix[allergen_ind] = vec![false; input.all_ingr.len()];
                continue;
            }
        }
    }
}
