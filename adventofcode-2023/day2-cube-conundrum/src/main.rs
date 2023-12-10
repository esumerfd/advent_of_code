use std::fs;
use std::str::FromStr;
use std::collections::HashMap;

struct Game {
    number: u32,
    bag_draws: Vec<HashMap<String, u32>>,
}

impl FromStr for Game {
    type Err = std::num::ParseIntError;

    fn from_str(game_text: &str) -> Result<Self, Self::Err> {

        if game_text.len() == 0 {
            return Ok(Game{number: 0, bag_draws: Vec::new()});
        }

        let (number, remainder) = parse_number(game_text);
        let bag_draws = parse_bag_draws(remainder);

        Ok(Game { number: number, bag_draws: bag_draws })
    }
}

fn main() {
    let _ = start_puzzle("./puzzle_input");
}

fn start_puzzle(puzzle_filename: &str) -> Vec<Game> {
    // Parse games, bag_draws
    let games = parse(puzzle_filename);
    // Filter by max of each color
    // sum game numbers of whats left

    return games;
}

fn parse(puzzle_filename: &str) -> Vec<Game> {
    let contents = fs::read_to_string(puzzle_filename);
    return parse_games(&contents.unwrap());
}

fn parse_games(contents: &str) -> Vec<Game> {
    let mut games: Vec<Game> = Vec::new();

    for line in contents.lines() {
        let game = parse_game(line);
        games.push(game);
    }

    return games;
}

fn parse_game(game: &str) -> Game {
    return Game::from_str(game).unwrap();
}

fn parse_number(game: &str) -> (u32, &str) {
    let line_parts = game.split(":").collect::<Vec<&str>>();
    let number_parts = line_parts[0].split(" ").collect::<Vec<&str>>();

    let number = number_parts[1].parse::<u32>().unwrap();
    return (number, line_parts[1]);
}

fn parse_bag_draws(bag_draws: &str) -> Vec<HashMap<String, u32>> {
    let mut results: Vec<HashMap<String, u32>> = Vec::new();

    let bag_draw = bag_draws.split(";");
    for cubes in bag_draw {

        let mut cube: HashMap<String, u32> = HashMap::new();

        let cube_colors = cubes.split(",").collect::<Vec<&str>>();
        for cube_color in cube_colors {

            let parts = cube_color.split(" ").collect::<Vec<&str>>();
            let count = parts[1];
            let color = parts[2];

            cube.insert(String::from(color), count.parse::<u32>().unwrap());
        }
        results.push(cube);
    }

    return results;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_read_file_lines() {
        let games = start_puzzle("./puzzle_input");

        assert_eq!(101, games.len());
    }

    #[test]
    fn should_parse_game() {
        let game: Game = parse_game("Game 79: 3 green, 1 blue, 2 red; 8 green, 1 blue, 2 red; 2 blue, 1 red, 11 green");

        assert_eq!(79, game.number);

        assert_eq!(3, game.bag_draws.len());

        assert_eq!(1, game.bag_draws[0]["blue"]);
        assert_eq!(3, game.bag_draws[0]["green"]);
        assert_eq!(2, game.bag_draws[0]["red"]);
        
        assert_eq!(1, game.bag_draws[1]["blue"]);
        assert_eq!(8, game.bag_draws[1]["green"]);
        assert_eq!(2, game.bag_draws[1]["red"]);
        
        assert_eq!(2, game.bag_draws[2]["blue"]);
        assert_eq!(11, game.bag_draws[2]["green"]);
        assert_eq!(1, game.bag_draws[2]["red"]);
    }

    #[test]
    fn should_parse_game_with_one_cube_only() {
        let game: Game = parse_game("Game 1: 3 green");

        assert_eq!(1, game.number);

        assert_eq!(1, game.bag_draws.len());

        assert_eq!(1, game.bag_draws[0].len());
        assert_eq!(3, game.bag_draws[0]["green"]);
    }
}
