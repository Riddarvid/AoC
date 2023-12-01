use std::fs::read_to_string;
use crate::days::day1::Day1;
use crate::utils::solvers::Solver;
use anyhow::{bail, Context, Result};

mod days;
pub mod utils;

fn main() -> Result<()> {
    let solution = solve_day(1)?;
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
    let solver = match day {
        1 => Day1,
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