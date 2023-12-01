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
        let first = find_first_digit(line)?;
        let last = find_last_digit(line)?;
        Some(first * 10 + last)
    }).sum()
}

fn solve2(lines: &[&str]) -> Option<u32> {
    lines.iter().map(|line| {
        let first = find_first_digit_literal(line)?;
        let last = find_last_digit_literal(line)?;
        Some(first * 10 + last)
    }).sum()
}

fn find_first_digit(input: &str) -> Option<u32> {
    for c in input.as_bytes() {
        if c.is_ascii_digit() {
            return (*c as char).to_digit(10);
        }
    }
    None
}

fn find_last_digit(input: &str) -> Option<u32> {
    for c in input.as_bytes().iter().rev() {
        if c.is_ascii_digit() {
            return (*c as char).to_digit(10);
        }
    }
    None
}

const LITERALS: [&str; 9] =
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

fn find_first_digit_literal(input: &str) -> Option<u32> {
    for i in 0..input.len() {
        let c = input.as_bytes()[i];
        if c.is_ascii_digit() {
            return (c as char).to_digit(10);
        }
        for (j, literal) in LITERALS.iter().enumerate() {
            if input[i..].starts_with(literal) {
                return Some(j as u32 + 1)
            }
        }
    }
    None
}

fn find_last_digit_literal(input: &str) -> Option<u32> {
    for i in (0..input.len()).rev() {
        let c = input.as_bytes()[i];
        if c.is_ascii_digit() {
            return (c as char).to_digit(10);
        }
        for (j, literal) in LITERALS.iter().enumerate() {
            if input[i..].starts_with(literal) {
                return Some(j as u32 + 1)
            }
        }
    }
    None
}