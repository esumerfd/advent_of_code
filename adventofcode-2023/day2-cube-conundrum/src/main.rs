use std::io;
use std::fs;
use std::str::FromStr;
use regex::Regex;

struct Game {
    number: u32,
    bag_draws: Vec<BagDraw>
}

impl FromStr for Game {
    type Err = std::num::ParseIntError;

    fn from_str(gameText: &str) -> Result<Self, Self::Err> {

        // https://regex101.com/r/pP17Oo/1
        let re = Regex::new(r"Game +(?P<number>\d+):((( *(?P<count>\d+) +(?P<color>blue|green|red)))[; ,]*)+").unwrap();

        let caps = re.captures(gameText).unwrap();
        println!(">>>> number {}", &caps["number"]);
        println!(">>>> count {}", &caps["count"]);
        println!(">>>> color {}", &caps["color"]);

        Ok(Game { number: 0, bag_draws: Vec::new() })
    }
}

struct BagDraw {
    blue: u32,
    green: u32,
    red: u32,
}

fn main() {
    start_puzzle("./puzzle_input")
}

fn start_puzzle(puzzle_filename: &str) {
    // Parse games, bag_draws
    parse(puzzle_filename);
    // Filter by max of each color
    // sum game numbers of whats left

}

fn parse(puzzle_filename: &str) -> io::Result<Game> {
    let contents = fs::read_to_string(puzzle_filename);
    parse_games(&contents.unwrap());
    return Ok(Game { number: 0, bag_draws: Vec::new()});
}

fn parse_games(games: &str) -> Vec<Game> {
    return vec!(Game { number: 79, bag_draws: vec![BagDraw{ blue: 1, green: 3, red: 2 }] });
}

fn parse_game(game: &str) -> Game {
    return Game::from_str(game).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_read_file_lines() {
        start_puzzle("./puzzle_input")
    }

    #[test]
    fn should_parse_game() {
        let game: Game = parse_game("Game 79: 3 green, 1 blue, 2 red; 8 green, 1 blue, 2 red; 2 blue, 1 red, 11 green");

        assert_eq!(79, game.number);
        assert_eq!(1, game.bag_draws[0].blue);
        assert_eq!(3, game.bag_draws[0].green);
        assert_eq!(2, game.bag_draws[0].red);
    }
}
