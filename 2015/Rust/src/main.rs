use std::fs;

mod day1;
mod day2;

fn main() {
    print_day(2).unwrap();
}

type Solution = (String, String);

fn print_day(day: u32) -> Result<(), String> {
    let solution = solve_day(day)?;
    print_result(solution);
    Ok(())
}

fn solve_day(day: u32) -> Result<Solution, String> {
    let input = read_input(day)?;
    let solve = match day {
        1 => { day1::solve }
        2 => { day2::solve }
        _ => { return Err(format!("No solver implemented for day {day}")); }
    };
    Ok(solve(input))
}

fn read_input(day: u32) -> Result<String, String> {
    let path = format!("inputs/input{day}.txt");
    fs::read_to_string(&path)
        .map_err(|error| { String::from(error.to_string()) })
}

fn print_result((part1, part2): Solution) {
    println!("Part1: {part1}");
    println!("Part2: {part2}")
}