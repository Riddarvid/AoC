package aoc21.days.day14;

import java.util.Objects;

public class Pair {
    private final char firstChar;
    private final char secondChar;

    public Pair(char firstChar, char secondChar) {
        this.firstChar = firstChar;
        this.secondChar = secondChar;
    }

    public char getFirstChar() {
        return firstChar;
    }

    public char getSecondChar() {
        return secondChar;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pair pair = (Pair) o;
        return firstChar == pair.firstChar && secondChar == pair.secondChar;
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstChar, secondChar);
    }

    @Override
    public String toString() {
        return "(" + firstChar + ", " + secondChar + ")";
    }
}
