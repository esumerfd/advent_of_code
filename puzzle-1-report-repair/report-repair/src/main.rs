use std::io; 
use std::fs;
use std::io::BufReader; 
use std::io::BufRead; 

fn main() {
    println!("Repair Report");

    let filename = String::from("../expense.report");

     let star_multiple = report_lines(filename)
        .and_then(|lines| Ok(process(lines)))
        .expect("Could not read file");

    println!("Found 2020 : {:}", star_multiple);
}

fn report_lines(filename: String) -> io::Result<Vec<String>> {
    let file_in = fs::File::open(filename)?;
    let file_reader = BufReader::new(file_in);

    return Ok(file_reader.lines().filter_map(io::Result::ok).collect())
}

fn process(lines: Vec<String>) -> i32 {
    for star in lines.iter() {
        let star_multiple = check_other(&lines, star);
        if star_multiple != 0 {
            return star_multiple;
        }
    }        
    return 0;
}

fn check_other(lines: &Vec<String>, star: &String) -> i32 {
    for other_star in lines.iter() {
        let int_star = star.parse::<i32>().unwrap();
        let int_other_star = other_star.parse::<i32>().unwrap();
        if find_2020(int_star, int_other_star) {
            return int_star * int_other_star; 
        }
    }

    return 0;
}

fn find_2020(star: i32, other_star: i32) -> bool {
    if star + other_star == 2020 {
        println!("{:} + {:} == 2020", star, other_star);
        return true;
    }

    return false;
}


