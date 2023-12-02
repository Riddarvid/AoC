use std::cmp::{max};
use regex::Regex;
use crate::utils::solvers::Solver;

pub struct Day2;

impl Solver for Day2 {
    fn solve(&self, input: &str) -> Option<(String, String)> {
        let min_max: Option<Vec<_>> = input.lines().map(find_max).collect();
        let min_max = min_max?;
        let part1 = solve1(&min_max);
        let part2 = solve2(&min_max);
        Some((part1.to_string(), part2.to_string()))
    }
}

type Colors = (u32, u32, u32);

fn find_max(line: &str) -> Option<Colors> {
    let mut max_vals = [0, 0, 0];

    let regex = Regex::new(r"\d+ (red|green|blue)").unwrap();
    for m in regex.find_iter(line) {
        let tokens: Vec<_> = m.as_str().split_whitespace().collect();
        let n: u32 = tokens.first()?.parse().ok()?;
        let color = tokens.get(1)?;
        let index = match *color {
            "red" => 0,
            "green" => 1,
            "blue" => 2,
            _ => { return None; }
        };
        max_vals[index] = max(max_vals[index], n);
    }

    Some((max_vals[0], max_vals[1], max_vals[2]))
}

fn solve1(min_max: &[Colors]) -> u32 {
    min_max.iter().enumerate()
        .map(|(id, (red, green, blue))| {
            if *red <= 12 && *green <= 13 && *blue <= 14 {
                id as u32 + 1
            } else {
                0
            }
        }).sum()
}

fn solve2(min_max: &[Colors]) -> u32 {
    min_max.iter().map(|(r, g, b)| r * g * b).sum()
}