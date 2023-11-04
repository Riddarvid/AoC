pub fn solve(input: String) -> (String, String) {
    let floor_changes = input.chars()
        .map(|c| { if c == '(' { 1 } else { -1 } });
    let part1 = solve1(floor_changes.clone());
    let part2 = solve2(floor_changes).unwrap();
    (part1.to_string(), part2.to_string())
}

fn solve1(floor_changes: impl Iterator<Item=i32>) -> i32 {
    floor_changes.sum()
}

fn solve2(floor_changes: impl Iterator<Item=i32>) -> Option<usize> {
    let mut sum = 0;
    floor_changes.enumerate().find(|(_, fc)| {
        sum += fc;
        sum == -1
    }).map(|(index, _)| index)
}
