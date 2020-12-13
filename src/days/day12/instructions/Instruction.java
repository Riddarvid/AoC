package days.day12.instructions;

import days.day12.Ship;

public abstract class Instruction {
    private final int value;

    public Instruction(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public abstract void apply1(Ship ship);

    public abstract void apply2(Ship ship);
}
