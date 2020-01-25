package aoc.days.day22;

public class Instruction {
    private final Type type;
    private long value;

    public Instruction(Type type, long value) {
        this.type = type;
        this.value = value;
    }

    public Instruction(Type type) {
        this.type = type;
    }

    public Type getType() {
        return type;
    }

    public long getValue() {
        return value;
    }
}
