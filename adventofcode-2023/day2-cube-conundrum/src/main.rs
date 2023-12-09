use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

type Callback = fn(&str);

fn main() {
    start_puzzle("./puzzle_input")
}

fn start_puzzle(puzzle_filename: &str) {
    games(puzzle_filename, bag_draws);
}

fn bag_draws(line: &str) {
    println!("{}", line);
}

fn games(puzzle_filename: &str, bag_draws: Callback) {
    if let Ok(lines) = read_lines(puzzle_filename) {
        for line in lines {
            if let Ok(ip) = line {
                bag_draws(&ip);
            }
        }
    }

}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Puzzle File: {}", err);
            std::process::exit(1);
        }
    };
    Ok(io::BufReader::new(file).lines())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_read_file_lines() {
        start_puzzle("./puzzle_input")
    }
}
