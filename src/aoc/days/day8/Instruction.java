package aoc.days.day8;

public class Instruction {
    private final Type type;
    private final int a;
    private final int b;

    public Instruction(Type type, int a, int b) {
        this.type = type;
        this.a = a;
        this.b = b;
    }

    public Type getType() {
        return type;
    }

    public int getA() {
        return a;
    }

    public int getB() {
        return b;
    }
}
