use crate::input::ParsedInput;

pub type Card = u8;
pub type Deck = Vec<Card>;

const MAX_CARD_VALUE: usize = 50;

struct GameState{
    pub deck_ptr: Vec<usize>,
    decks: Vec<Deck>,
    past_decks: Vec<Vec<Deck>>,
    winner: Option<usize>,
}

impl GameState{
    pub fn new(decks: Vec<Deck>) -> GameState{
        GameState {
            deck_ptr: vec![0; decks.len()],
            past_decks: decks.iter().map(|d| vec![d.clone()]).collect(),
            decks: decks,
            winner: None,
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

    pub fn decide_winner(&mut self) -> usize {
        let card_p0 = self.get_card_player(0);
        let card_p1 = self.get_card_player(1);
        if card_p0 > card_p1 { 0 } else if card_p0 < card_p1 { 1 } else { panic!("Equality") }
    }

    pub fn populate_deck(&self, player: usize, deck: &mut Deck, size: usize, skip: usize){
        for ncard in 0..size{
            let mut ind = self.deck_ptr[player] + ncard + skip;
            if ind >= self.decks[player].len(){
                ind -= self.decks[player].len();
            }
            deck.push(self.decks[player][ind]);
        }
    }

    pub fn check_recursion_loop(&mut self) -> bool {
        let mut recloop = true;
        for n in 0..self.decks.len(){
            let mut deck = vec![];
            self.populate_deck(n, &mut deck, self.decks[n].len(), 0);
            if !self.past_decks[n].contains(&deck){
                recloop = false;
            }
            self.past_decks[n].push(deck.clone());
        }
        recloop
    }

    pub fn play_recursive_game(&mut self, p1_decksz: usize, p2_decksz: usize) -> (usize, usize){
        println!("--- Recursive game ---");
        let mut decks = vec![vec![], vec![]];
        self.populate_deck(0, &mut decks[0], p1_decksz, 1);
        println!("Deck player 1: {:?}", decks[0]);
        self.populate_deck(1, &mut decks[1], p2_decksz, 1);
        println!("Deck player 2: {:?}", decks[1]);

        let mut state = GameState::new(decks);
        while !state.round() {}
        state.find_winner();
        println!("--- Winner: {:?} ---", state.winner);
        if let Some(winner) = state.winner{
            if winner == 0{
                (0, 1)
            } else {
                (1, 0)
            }
        }else{
            unreachable!()
        }
    }

    pub fn round(&mut self) -> bool {
        for ptr in 0..self.deck_ptr.len(){
            self.deck_ptr[ptr] = self.deck_ptr[ptr] % self.decks[ptr].len();
        }

        let card_p0 = self.get_card_player(0);
        let card_p1 = self.get_card_player(1);
 
        println!("");
        println!("Player 1 deck: {:?}", self.decks[0]);
        println!("Player 1 card: {}", card_p0);
        println!("Player 2 deck: {:?}", self.decks[1]);
        println!("Player 2 card: {}", card_p1);
        if ((self.decks[0].len()-1) >= (card_p0 as usize))
            & ((self.decks[1].len()-1) >= (card_p1 as usize)){
            let (winner, loser) = self.play_recursive_game(card_p0 as usize, card_p1 as usize);
            let card = self.decks[loser].remove(self.deck_ptr[loser]);
            self.receive_card(winner, card);
        } else {
            if card_p0 > card_p1 {
                let card = self.decks[1].remove(self.deck_ptr[1]);
                self.receive_card(0, card);
            } else if card_p0 < card_p1 {
                let card = self.decks[0].remove(self.deck_ptr[0]);
                self.receive_card(1, card);
            } else {
                panic!("Equality");
            }
        }
        
        if self.check_recursion_loop(){
            self.winner = Some(0);
            return true;
        }
        self.decks.iter().any(|d| d.len() == 0)
    }

    pub fn find_winner(&mut self){
        if let Some(winner) = self.winner{
            return;
        }
        for player in 0..self.decks.len(){
            if self.decks[player].len() > 0 {
                self.winner = Some(player);
                return;
            }
        }
        panic!("No winner found");
    }

    pub fn calc_score(&mut self) -> usize{
        if self.winner == None{
            self.find_winner();
        }

        if let Some(winner) = self.winner{
            let mut score = 0;
            let mut ptr = self.deck_ptr[winner];
            for ncard in 0..self.decks[winner].len(){
                if ptr == 0 {
                    ptr = self.decks[winner].len() -1;
                } else {
                    ptr -= 1;
                }
                let card = self.decks[winner][ptr] as usize;
                println!("Score + {} * {}", 1+ncard, card);
                score += (1+ncard) * card;
            }
            score
        } else {
            unreachable!();
        }
    }
}

pub fn solve(input: ParsedInput){
    let mut state = GameState::new(input.decks);
    while !state.round() {}
    println!("Score: {}", state.calc_score());
}
