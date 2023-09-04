pub fn solve(input: String) -> (String, String) {
    let floor_changes = input.chars()
        .map(|c| { if c == '(' { 1 } else { -1 } })
        .collect();
    let part1 = solve1(&floor_changes);
    let part2 = solve2(&floor_changes);
    (part1.to_string(), part2.to_string())
}

fn solve1(floor_changes: &Vec<i32>) -> i32 {
    floor_changes.iter().sum()
}

fn solve2(floor_changes: &Vec<i32>) -> usize {
    let mut sum = 0;
    for (i, fc) in floor_changes.iter().enumerate() {
        sum += fc;
        if sum == -1 { return i + 1; }
    };
    panic!("Basement never reached.")
}
