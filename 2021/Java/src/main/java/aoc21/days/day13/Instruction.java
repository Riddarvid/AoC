package aoc21.days.day13;

public class Instruction {
    public Instruction(Direction direction, int value) {
        this.direction = direction;
        this.value = value;
    }

    private final Direction direction;
    private final int value;

    public Direction getDirection() {
        return direction;
    }

    public int getValue() {
        return value;
    }
}
