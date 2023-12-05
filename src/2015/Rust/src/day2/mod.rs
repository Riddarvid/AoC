pub fn solve(input: String) -> (String, String) {
    let presents = input.lines().map(parse_present);
    let part1 = solve1(presents.clone());
    let part2 =  solve2(presents);
    (part1.to_string(), part2.to_string())
}

// Input parsing

fn parse_present(input: &str) -> Present {
    let values: Vec<_> = input
        .split('x')
        .map(|str| {
            str.parse().unwrap()
        })
        .collect();
    Present {
        length: values[0],
        width: values[1],
        height: values[2],
    }
}

struct Present {
    length: u32,
    width: u32,
    height: u32,
}

// Part 1

fn solve1(presents: impl Iterator<Item=Present>) -> u32 {
    presents.map(|p| find_area(&p)).sum()
}

fn find_area(present: &Present) -> u32 {
    let areas = vec![
        present.length * present.width,
        present.width * present.height,
        present.height * present.length,
    ];
    let base_area: u32 = areas.iter().map(|area| area * 2).sum();
    let min_area = areas.iter().min().unwrap();
    base_area + min_area
}

// Part 2

fn solve2(presents: impl Iterator<Item=Present>) -> u32 {
    presents.map(|p| find_ribbon_length(&p)).sum()
}

fn find_ribbon_length(present: &Present) -> u32 {
    let mut sides = vec![present.length, present.width, present.height];
    sides.sort();
    let ribbon_length = 2 * sides[0] + 2 * sides[1];
    let bow_length: u32 = sides.iter().product();
    ribbon_length + bow_length
}