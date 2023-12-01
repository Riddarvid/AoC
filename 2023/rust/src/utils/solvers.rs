pub trait Solver {
    fn solve(&self, input: &str) -> Option<(String, String)>;
}