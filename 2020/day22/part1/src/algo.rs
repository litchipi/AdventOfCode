use crate::input::ParsedInput;

pub type Card = u8;
pub type Deck = Vec<Card>;

struct GameState{
    pub deck_ptr: Vec<usize>,
    decks: Vec<Deck>,
}

impl GameState{
    pub fn new(decks: Vec<Deck>) -> GameState{
        GameState {
            deck_ptr: vec![0; decks.len()],
            decks: decks,
        }
    }

    pub fn get_card_player(&self, nplayer: usize) -> Card {
        assert!(nplayer < self.decks.len());
        self.decks[nplayer][self.deck_ptr[nplayer]]
    }

    pub fn receive_card(&mut self, receiver: usize, card: Card){
        self.decks[receiver].insert(self.deck_ptr[receiver]+1, card);
        self.deck_ptr[receiver] += 2;
    }

    pub fn round(&mut self) -> bool {
        for ptr in 0..self.deck_ptr.len(){
            self.deck_ptr[ptr] = self.deck_ptr[ptr] % self.decks[ptr].len();
        }

        let card_p0 = self.get_card_player(0);
        let card_p1 = self.get_card_player(1);
        
        if card_p0 > card_p1 {
            let card = self.decks[1].remove(self.deck_ptr[1]);
            self.receive_card(0, card);
        } else if card_p0 < card_p1 {
            let card = self.decks[0].remove(self.deck_ptr[0]);
            self.receive_card(1, card);
        } else {
            panic!("Equality");
        }

        self.decks.iter().any(|d| d.len() == 0)
    }

    pub fn calc_score(&self) -> usize{
        for player in 0..self.decks.len(){
            if self.decks[player].len() > 0 {
                let mut score = 0;

                let mut ptr = self.deck_ptr[player];
                for ncard in 0..self.decks[player].len(){
                    if ptr == 0 {
                        ptr = self.decks[player].len() -1;
                    } else {
                        ptr -= 1;
                    }
                    let card = self.decks[player][ptr] as usize;
                    score += (1+ncard) * card;
                }
                return score;
            }
        }
        panic!("Not winner found");
    }
}

pub fn solve(input: ParsedInput){
    let mut state = GameState::new(input.decks);
    while !state.round() {}
    println!("Score: {}", state.calc_score());
}
