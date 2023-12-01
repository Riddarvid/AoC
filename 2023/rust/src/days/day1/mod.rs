use crate::utils::solvers::Solver;


#[derive(Default)]
pub struct Day1;


impl Solver for Day1 {
    fn solve(&self, input: &str) -> (String, String) {
        (input.to_string(), "".to_string())
    }
}