package days.day8.instructions;

import days.day8.Console;

public class Acc implements Instruction {
    private final int change;

    public Acc(int change) {
        this.change = change;
    }

    @Override
    public void apply(Console console) {
        console.setAccumulator(console.getAccumulator() + change);
        console.incPc();
    }

    @Override
    public String getName() {
        return "acc";
    }

    @Override
    public int getValue() {
        return change;
    }
}
