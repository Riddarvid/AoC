use crate::days::day1::Order::{First, Last};
use crate::utils::solvers::Solver;

pub struct Day1;

impl Solver for Day1 {
    fn solve(&self, input: &str) -> Option<(String, String)> {
        let input: Vec<&str> = input.lines().collect();
        let part1 = solve1(&input)?;
        let part2 = solve2(&input)?;
        Some((part1.to_string(), part2.to_string()))
    }
}

fn solve1(lines: &[&str]) -> Option<u32> {
    lines.iter().map(|line| {
        let first = find_digit(line, First, false)?;
        let last = find_digit(line, Last, false)?;
        Some(first * 10 + last)
    }).sum()
}

fn solve2(lines: &[&str]) -> Option<u32> {
    lines.iter().map(|line| {
        let first = find_digit(line, First, true)?;
        let last = find_digit(line, Last, true)?;
        Some(first * 10 + last)
    }).sum()
}


const LITERALS: [&str; 9] =
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

#[derive(Eq, PartialEq)]
enum Order {
    First,
    Last,
}

fn find_digit(input: &str, order: Order, accept_literal: bool) -> Option<u32> {
    let mut range: Vec<usize> = (0..input.len()).collect();
    if order == Last {
        range.reverse();
    }
    for i in range {
        if let Some(d) = parse_digit(&input[i..]) {
            return Some(d);
        }
        if accept_literal {
            if let Some(d) = parse_literal(&input[i..]) {
                return Some(d);
            }
        }
    }
    None
}

fn parse_digit(input: &str) -> Option<u32> {
    input.chars().next()?.to_digit(10)
}

fn parse_literal(input: &str) -> Option<u32> {
    for (i, literal) in LITERALS.iter().enumerate() {
        if input.starts_with(literal) {
            return Some(i as u32 + 1);
        }
    }
    None
}