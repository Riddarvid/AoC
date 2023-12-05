package aoc21.days.day18;

public abstract class SnailNumber {
    protected static boolean hasReduced;

    public abstract SnailNumber tryExplode(int depth, LeftToRightReference reference);

    public abstract SnailNumber trySplit();

    public abstract void register(LeftToRightReference reference);

    protected abstract String toStringColored(int depth);

    public abstract long getMagnitude();

    public abstract SnailNumber copy();
}
