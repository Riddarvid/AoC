package days.day8.instructions;

import days.day8.Console;

public class Nop implements Instruction {
    private final int value;

    public Nop(int value) {
        this.value = value;
    }

    @Override
    public void apply(Console console) {
        console.incPc();
    }

    @Override
    public String getName() {
        return "nop";
    }

    @Override
    public int getValue() {
        return value;
    }
}
