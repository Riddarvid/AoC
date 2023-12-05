use std::collections::{HashMap, HashSet};
use std::iter::Map;
use std::str::Chars;

pub fn solve(input: &str) -> (String, String) {
    let instructions = input
        .lines()
        .next().unwrap()
        .chars()
        .map(parse_dir);
    let part1 = visit_houses(instructions.clone()).len();
    let part2 = solve2(instructions);
    (part1.to_string(), part2.to_string())
}

fn solve2(instructions: impl Iterator<Item=Dir>) -> usize {
    let santa = instructions.clone()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, dir)| dir);
    let robo_santa = instructions
        .enumerate()
        .filter(|(i, _)| i % 2 == 1)
        .map(|(_, dir)| dir);
    let santa_houses = visit_houses(santa);
    let robo_houses = visit_houses(robo_santa);
    let houses = santa_houses.union(&robo_houses);
    houses.count()
}

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

fn parse_dir(c: char) -> Dir {
    match c {
        '^' => Dir::Up,
        'v' => Dir::Down,
        '<' => Dir::Left,
        '>' => Dir::Right,
        _ => panic!("invalid token {c}")
    }
}

fn visit_houses(instructions: impl Iterator<Item=Dir>) -> HashSet<(i32, i32)> {
    let mut houses = HashSet::new();
    let mut pos = (0, 0);
    houses.insert(pos);
    for dir in instructions {
        pos = move_pos(pos, dir);
        houses.insert(pos);
    };
    houses
}

fn move_pos((x, y): (i32, i32), dir: Dir) -> (i32, i32) {
    let (x1, y1) = match dir {
        Dir::Up => (0, 1),
        Dir::Down => (0, -1),
        Dir::Left => (-1, 0),
        Dir::Right => (1, 0),
    };
    (x + x1, y + y1)
}