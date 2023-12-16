use std::io; 
use std::fs;
use std::io::BufReader; 
use std::io::BufRead; 
use combinations::Combinations;

fn main() {
    println!("Repair Report");

    let filename = String::from("expense.report");
    let stars = report_lines(filename).unwrap();

    combinations(stars, 3, |combination| check(combination));
}

fn report_lines(filename: String) -> io::Result<Vec<i32>> {
    let file_in = fs::File::open(filename)?;
    let file_reader = BufReader::new(file_in);

    let lines: Vec<String> = file_reader.lines().filter_map(io::Result::ok).collect();
    let expenses: Vec<i32> = lines.iter().map(|line| line.parse::<i32>().unwrap()).collect();

    return Ok(expenses);
}

fn check(combination: &Vec<i32>) -> bool {
    println!("combiation {:?}", combination);
    return true;
}

fn combinations <F>(stars: Vec<i32>, number: usize, processor: F) where F: Fn(&Vec<i32>) -> bool {

    let mut selected: Vec<_> = Vec::new();

    let combinations: Vec<_> = Combinations::new(stars, number).collect();
    combinations.iter().for_each(|combination| {
        if processor(combination) {
            selected.push(combination);
        }
    });

    return selected;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_call_function() {
        combinations(vec![1,2,3,4,5], 2, |combination| true);
    }
}
