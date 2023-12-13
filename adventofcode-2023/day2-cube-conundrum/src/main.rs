use std::fs;
use std::str::FromStr;
use std::collections::HashMap;

type Bag = HashMap<String, u32>;

struct Game {
    number: u32,
    bag_draws: Vec<Bag>,
}

impl Clone for Game {
    fn clone(&self) -> Self {
        return Game {
            number: self.number,
            bag_draws: self.bag_draws.clone(),
        };
    }
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

impl Game {
    fn sum(games: &mut Vec<Game>) -> u32 {
        return games.iter().map(|game| game.number).sum();
    }

    fn power_sum(games: &mut Vec<Game>) -> u32 {

        let mut power_sums: u32 = 0;
        let mut smallest_bag: Bag;
        for game in games {
            smallest_bag = Game::smallest_bag(game.clone());
            power_sums += Game::color_multiple(smallest_bag);
        }

        return power_sums;
    }

    fn color_multiple(bag: Bag) -> u32 {
        let mut multiple = 1;
        for (_, count) in bag {
            multiple *= count;
        }
        return multiple;
    }

    fn smallest_bag(game: Game) -> Bag {
        let mut smallest_bag: Bag = parse_bag(" 0 blue, 0 green, 0 red");

        for bag in game.bag_draws.clone() {
            for (color, count) in bag.clone() {
                if bag.contains_key(&color) && count > smallest_bag[&color] {
                    smallest_bag.insert(color, count);
                }
            }
        }

        return smallest_bag;
    }
}

fn main() {
    let bag = parse_bag(" blue: 14, green: 13, red: 12");
    let (sum, power_sum) = start_puzzle("./puzzle_input", bag);
    println!("Game sum: {}, power_sum: {}", sum, power_sum);
}

fn start_puzzle(puzzle_filename: &str, bag: Bag) -> (u32, u32) {
    // Parse games, bag_draws
    let mut games = parse(puzzle_filename);

    // Filter by max of each color
    let mut filtered_games = filter(games.clone(), bag);
    // sum game numbers of whats left

    return ( Game::sum(&mut filtered_games), Game::power_sum(&mut games) )
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

fn parse_bag_draws(bag_draws: &str) -> Vec<Bag> {
    let mut results: Vec<Bag> = Vec::new();

    let bag_draw = bag_draws.split(";");
    for cubes in bag_draw {

        let cube = parse_bag(cubes);
        results.push(cube);
    }

    return results;
}

fn parse_bag(cubes: &str) -> Bag {
    let mut cube: Bag = Bag::new();

    let cube_colors = cubes.split(",").collect::<Vec<&str>>();
    for cube_color in cube_colors {

        let parts = cube_color.split(" ").collect::<Vec<&str>>();
        let count = parts[1];
        let color = parts[2];

        cube.insert(String::from(color), count.parse::<u32>().unwrap());
    }

    return cube;
}

fn filter(games: Vec<Game>, bag: Bag) -> Vec<Game> {
    let mut filtered: Vec<Game> = Vec::new();

    for game in games {

        let large_draws = game.bag_draws
            .clone()
            .into_iter()
            .filter(|draw| {
                return cube_count(draw, "blue") > cube_count(&bag, "blue") 
                || cube_count(draw, "green") > cube_count(&bag, "green") 
                || cube_count(draw, "red") > cube_count(&bag, "red")
            })
            .count();

        if large_draws == 0 {
            filtered.push(game);
        }
    }

    return filtered;
}

fn cube_count(bag: &Bag, color: &str) -> u32 {
    let mut count: u32 = 0;
    if bag.contains_key(color) {
        count = bag[color];
    }
    return count;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_add_all_game_numbers_not_filtered() {
        let bag = parse_bag(" 14 blue, 13 green, 12 red");
        let (sum, power_sum) = start_puzzle("./puzzle_input", bag);

        assert_eq!(2476, sum);
        assert_eq!(54911, power_sum);
    }

    #[test]
    fn should_read_file_lines() {
        let games = parse("./puzzle_input");

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

    #[test]
    fn should_filter_out_game_that_doesnt_match() {
        let games: Vec<Game> = vec!(
            parse_game("Game 5: 2 blue, 2 green, 3 red"),
            parse_game("Game 9: 1 blue, 2 green, 3 red"),
        );

        let bag = parse_bag(" 1 blue, 2 green, 3 red");

        let filtered_games = filter(games, bag);

        assert_eq!(1, filtered_games.len());
        assert_eq!(9, filtered_games[0].number);
    }

    #[test]
    fn should_filter_out_draw_specified_second() {
        let games: Vec<Game> = vec!(
            parse_game("Game 5: 1 blue, 2 green, 3 red"),
            parse_game("Game 9: 1 blue, 2 green, 3 red; 1 blue, 3 green, 3 red"),
        );

        let bag = parse_bag(" 1 blue, 2 green, 3 red");

        let filtered_games = filter(games, bag);

        assert_eq!(1, filtered_games.len());
        assert_eq!(5, filtered_games[0].number);
    }

    #[test]
    fn should_sum_all_game_numbers() {
        let mut games: Vec<Game> = vec!(
            parse_game("Game 79: 3 green, 1 blue, 2 red); 8 green, 1 blue, 2 red; 2 blue, 1 red, 11 green"),
            parse_game("Game 88: 3 green, 1 blue, 2 red); 8 green, 1 blue, 2 red; 2 blue, 1 red, 11 green"),
        );

        assert_eq!(167, Game::sum(&mut games));
    }

    #[test]
    fn should_sum_all_games_when_there_are_none() {
        let mut games: Vec<Game> = Vec::new();

        assert_eq!(0, Game::sum(&mut games));
    }

    #[test]
    fn should_find_least_bag_cubes_of_games() {
        let mut games: Vec<Game> = vec!(
            parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
            parse_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"),
            parse_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"),
            parse_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"),
            parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"),
        );

        assert_eq!(2286, Game::power_sum(&mut games));
    }

    #[test]
    fn should_find_least_cubes_of_game() {
        let game = parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green");

        let bag = Game::smallest_bag(game);

        assert_eq!(6, bag["blue"]);
        assert_eq!(2, bag["green"]);
        assert_eq!(4, bag["red"]);
    }

    #[test]
    fn should_multiply_color_counts_together() {
        let bag: Bag = parse_bag(" 2 blue, 3 green, 4 red");

        assert_eq!(24, Game::color_multiple(bag));
    }
}
