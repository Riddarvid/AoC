package aoc21.days.day18;

import java.util.Objects;

public class RegularNumber extends SnailNumber {
    private int value;

    public RegularNumber(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public void add(int toAdd) {
        value += toAdd;
    }

    private Pair split() {
        int leftValue = (int) Math.floor(value / 2.0);
        int rightValue = (int) Math.ceil(value / 2.0);
        return new Pair(new RegularNumber(leftValue), new RegularNumber(rightValue));
    }

    @Override
    public SnailNumber tryExplode(int depth, LeftToRightReference reference) {
        reference.increaseIndex();
        return this;
    }

    @Override
    public SnailNumber trySplit() {
        if (value >= 10) {
            hasReduced = true;
            return split();
        }
        return this;
    }

    @Override
    public long getMagnitude() {
        return value;
    }

    @Override
    public void register(LeftToRightReference reference) {
        reference.register(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RegularNumber that = (RegularNumber) o;
        return value == that.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    protected String toStringColored(int depth) {
        if (value >= 10) {
            return ColorCodes.colorCodeMap.get(4) + value + ColorCodes.RESET;
        }
        return "" + value;
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public SnailNumber copy() {
        return new RegularNumber(value);
    }
}
