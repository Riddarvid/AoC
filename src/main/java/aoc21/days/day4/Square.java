package aoc21.days.day4;

import java.util.Objects;

public class Square {
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";

    private final int number;
    private boolean marked;

    public Square(int number) {
        this.number = number;
        this.marked = false;
    }

    public int getNumber() {
        return number;
    }

    public boolean isMarked() {
        return marked;
    }

    public void mark() {
        marked = true;
    }

    @Override
    public String toString() {
        String numberString = "";
        if (number < 10) {
            numberString = " ";
        }
        numberString += number;
        if (isMarked()) {
            return ANSI_RED + numberString + ANSI_RESET;
        } else {
            return numberString;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Square square = (Square) o;
        return number == square.number && marked == square.marked;
    }

    @Override
    public int hashCode() {
        return Objects.hash(number, marked);
    }
}
