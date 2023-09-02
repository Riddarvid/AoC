package aoc21.days.day2;

public abstract class Engine {
    protected long horizontalPosition;
    protected long depth;

    protected abstract void move(Instruction instruction);

    public long getHorizontalPosition() {
        return horizontalPosition;
    }

    public long getDepth() {
        return depth;
    }
}
