package aoc21.days.day21;

public class DeterministicDice implements Dice {
    private int count;

    public DeterministicDice() {
        count = 0;
    }

    @Override
    public int roll() {
        count++;
        return count;
    }

    @Override
    public int getNumberOfRolls() {
        return count;
    }
}
