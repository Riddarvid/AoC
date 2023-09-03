mod day1;
mod day2;

fn main() {
    run_day(1);
    run_day(2);
}

fn run_day(day: u32) {
    let dummy_input = String::from("");
    let result = match day {
        1 => {day1::solve(dummy_input)}
        2 => {day2::solve(dummy_input)}
        _ => {panic!("Day not found")}
    };
    print_result(result);
}

fn print_result((part1, part2): (String, String)) {
    println!("Part1: {part1}\nPart2: {part2}");
}