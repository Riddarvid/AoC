pub trait Solver {
    fn solve(&self, input: &str) -> (String, String);
}