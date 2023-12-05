package aoc15.days.day3;

import java.util.Objects;

public class House {
    private int x;
    private int y;
    private int presents;

    public House(int x, int y) {
        this(x, y, 1);
    }

    public House(int x, int y, int presents) {
        this.x = x;
        this.y = y;
        this.presents = presents;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        House house = (House) o;
        return x == house.x &&
                y == house.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    @Override
    public String toString() {
        return "Pos: " + x + ", " + y + " Presents: " + presents;
    }

    void addPresent() {
        presents++;
    }
}
