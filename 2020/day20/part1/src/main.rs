#![allow(dead_code, unused_variables)]
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

type TileID = u64;

#[derive(Copy, Clone)]
enum SIDES {
    TopSide,
    BottomSide,
    RightSide,
    LeftSide
}


// TopRight -> BottomRight
// TopRight -> TopLeft
// RightBottom -> TopRight
// LeftBottom -> OK

#[derive(Copy, Clone, Debug)]
struct Tile {
    id: TileID,
    borders_found: u8,  //Bottom Left Right Top
    data : [u16; 10],
    id_borders : [TileID; 4],
}

impl Tile{

    fn compare_border(&mut self, borders: [u16; 4], id: TileID){
        for nb in 0..4{         //Nb border
            for nrot in 0..4{
                let normal_found = if borders[nb] == self.get_border((nb+nrot)%4) { 1<<((nb+nrot)%4) } else { 0 };
                let reverse_found= if borders[nb] == (self.get_border((nb+nrot)%4).reverse_bits() >> (16-10)) { 1<<((nb+nrot)%4) } else { 0 };
                self.borders_found |= normal_found;
                self.borders_found |= reverse_found;
                if (normal_found | reverse_found) > 0 {
                    assert_eq!(self.id_borders[(nb+nrot)%4], 0);
                    self.id_borders[(nb+nrot)%4] = id;
                }
            }
        }
    }

    fn count_borders(&self) -> u8{
               self.borders_found.count_ones() as u8
    }

    fn get_all_borders(&self) -> [u16; 4]{
        [self.get_border(0), self.get_border(1), self.get_border(2), self.get_border(3)]
    }

    fn get_border(&self, nb : usize) -> u16{
        match nb{
            0 => self.get_top_border(),
            1 => self.get_right_border(),
            2 => self.get_bottom_border(),
            3 => self.get_left_border(),
            _ => panic!("Asked for a border that doesn't exist")
        }
    }

    fn get_left_border(&self) -> u16{
        let mut res: u16 = 0;
        for (i, d) in self.data.iter().enumerate() {
            res += (d >> 9) << i;
        }
        res
    }

    fn get_right_border(&self) -> u16{
        let mut res: u16 = 0;
        for (i, d) in self.data.iter().enumerate() {
            res += (d%2) << i;
        }
        res
    }

    fn get_top_border(&self) -> u16{
        self.data[0]
    }

    fn get_bottom_border(&self) -> u16{
        self.data[self.data.len()-1]
    }

    fn print_data(&self){
        println!("\nTile {}:", self.id);
        for d in self.data.iter(){
            println!("{:10b}", d);
        }
        println!("");
    }
}

fn match_tiles(tiles: &mut Vec<Tile>) -> [TileID; 4] {

    for ntile in 0..tiles.len(){
        for ncmp in 0..tiles.len(){
            if ntile == ncmp{
                continue;
            }
            let borders = tiles[ncmp].get_all_borders();
            let cmp_id = tiles[ncmp].id;
            tiles[ntile].compare_border(borders, cmp_id);
        }
        assert_ne!(tiles[ntile].borders_found, 0);
    }


    let tiles_borders = tiles.iter().filter(|x| x.count_borders() == 2).collect::<Vec<&Tile>>();
    [tiles_borders[0].id, tiles_borders[1].id, tiles_borders[2].id, tiles_borders[3].id]
}

/*
New tile of ID 2311
0011010010
1100100000
1000110010
1111010001
1101101110
1100010111
0101010011
0010000100
1110001010
0011100111
*/
/*
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
*/

// Parsing
fn get_id_from_line(line: &str) -> TileID{
    line.split("Tile ").collect::<Vec<&str>>()[1].split(":").collect::<Vec<&str>>()[0].parse::<TileID>().unwrap() //.except("Cannot parse Tile ID")
}

fn line_to_data(line: &str) -> u16{
    let mut weight = 9;
    let mut res = 0;
    for ch in line.chars().into_iter() {
        res += match ch {
            '.' => 0,
            '#' => 1 << weight,
            _ => panic!("Tile parsing error, char not recognized")
        };
        weight -= 1;
    }
    res
}

fn get_input(fname: impl AsRef<Path>) -> Vec<Tile> {
    let file = File::open(fname).expect("no such file");
    let buf = BufReader::new(file);
    let flines : Vec<String> = buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let mut current_tile = Tile{id:0, borders_found:0, data:[0;10], id_borders:[0;4]};
    let mut tiles : Vec<Tile> = [].to_vec();
    let mut nline = 0;
    for line in flines.iter() {
        if nline == 0{
            current_tile = Tile{id:get_id_from_line(line), borders_found:0, data:[0;10], id_borders:[0;4]};
            nline += 1;
        } else if nline <= 10 {
            assert_ne!(current_tile.id, 0);
            current_tile.data[nline-1] = line_to_data(line);
            nline += 1;
        } else if line.len() > 0 {
            println!("Wrong line: {}", line);
            panic!("Number of line not supposed to be > 10")
        } else {
            tiles.push(current_tile.clone());
            nline = 0;
        }
    }
    tiles.push(current_tile.clone());
    tiles
}

fn start(fname: &str) {
    let mut tiles = get_input(fname);
    let borders = match_tiles(&mut tiles);
    println!("Corners: {} {} {} {}", borders[0], borders[1], borders[2], borders[3]);
    let res : u64 = borders[0] * borders[1] * borders[2] * borders[3];
    println!("Result: {}", res);
}

fn help_msg(){
    println!("Usage: day20 <fname>");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => start(args[1].as_str()),
        _ => help_msg(),
    };
}
