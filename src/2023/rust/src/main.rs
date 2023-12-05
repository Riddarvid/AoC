use std::fs::read_to_string;
use crate::days::day1::Day1;
use crate::utils::solvers::Solver;
use anyhow::{bail, Context, Result};
use crate::days::day2::Day2;
use crate::days::day3::Day3;

mod days;
pub mod utils;

fn main() -> Result<()> {
    let solution = solve_day(3)?;
    print_solution(solution);
    Ok(())
}

fn print_solution((part1, part2): (String, String)) {
    println!("Part1:");
    println!("{part1}");
    println!();
    println!("Part2:");
    println!("{part2}");
}

fn solve_day(day: u32) -> Result<(String, String)> {
    let input = read_input(day)?;
    let solver: Box<dyn Solver> = match day {
        1 => Box::new(Day1),
        2 => Box::new(Day2),
        3 => Box::new(Day3),
        _ => bail!("Solution not yet implemented for day {day}")
    };
    let result = solver.solve(&input).context("No solution found")?;
    Ok(result)
}

fn read_input(day: u32) -> Result<String> {
    let path = format!("input/input{day}.txt");
    let input = read_to_string(path)?;
    Ok(input)
}