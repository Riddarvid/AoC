package aoc21.days.day21;

import java.util.List;

public class DiracDice implements Dice {
    private final List<Integer> sequence;
    private int count;

    public DiracDice(List<Integer> sequence) {
        this.sequence = sequence;
        count = 0;
    }

    @Override
    public int roll() {
        int result = sequence.get(count);
        count++;
        return result;
    }

    @Override
    public int getNumberOfRolls() {
        return count;
    }
}
