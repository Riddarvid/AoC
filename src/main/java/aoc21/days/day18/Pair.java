package aoc21.days.day18;

import java.util.Objects;

import static aoc21.days.day18.ColorCodes.RESET;
import static aoc21.days.day18.ColorCodes.colorCodeMap;

public class Pair extends SnailNumber {
    private SnailNumber firstNumber;
    private SnailNumber secondNumber;

    public Pair(SnailNumber firstNumber, SnailNumber secondNumber) {
        this.firstNumber = firstNumber;
        this.secondNumber = secondNumber;
    }

    private SnailNumber explode(LeftToRightReference reference) {
        hasReduced = true;
        int firstValue = ((RegularNumber) firstNumber).getValue();
        int secondValue = ((RegularNumber) secondNumber).getValue();
        reference.explode(firstValue, secondValue);
        return new RegularNumber(0);
    }

    @Override
    public SnailNumber tryExplode(int depth, LeftToRightReference reference) {
        depth++;
        if (depth >= 5) {
            return explode(reference);
        }
        SnailNumber firstReduced = firstNumber.tryExplode(depth, reference);
        if (hasReduced) {
            firstNumber = firstReduced;
            return this;
        }
        SnailNumber secondReduced = secondNumber.tryExplode(depth, reference);
        if (hasReduced) {
            secondNumber = secondReduced;
            return this;
        }
        return this;
    }

    @Override
    public SnailNumber trySplit() {
        SnailNumber firstReduced = firstNumber.trySplit();
        if (hasReduced) {
            firstNumber = firstReduced;
            return this;
        }
        SnailNumber secondReduced = secondNumber.trySplit();
        if (hasReduced) {
            secondNumber = secondReduced;
            return this;
        }
        return this;
    }

    @Override
    public long getMagnitude() {
        return 3 * firstNumber.getMagnitude() + 2 * secondNumber.getMagnitude();
    }

    @Override
    public void register(LeftToRightReference reference) {
        firstNumber.register(reference);
        secondNumber.register(reference);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pair pair = (Pair) o;
        return Objects.equals(firstNumber, pair.firstNumber) && Objects.equals(secondNumber, pair.secondNumber);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstNumber, secondNumber);
    }

    public String toStringColored(int depth) {
        String colorCode = colorCodeMap.getOrDefault(depth, colorCodeMap.get(4));
        return colorCode + "[" + RESET + firstNumber.toStringColored(depth + 1) + "," +
                secondNumber.toStringColored(depth + 1) + colorCode + "]" + RESET;
    }

    @Override
    public String toString() {
        return "[" + firstNumber.toString() + "," + secondNumber.toString() + "]";
    }

    @Override
    public SnailNumber copy() {
        return new Pair(firstNumber.copy(), secondNumber.copy());
    }
}
