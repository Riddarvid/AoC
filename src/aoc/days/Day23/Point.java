package aoc.days.Day23;

import java.util.Objects;

public class Point {
    int x;
    int y;
    int z;
    private int botsInRange;

    public Point(int x, int y, int z) {
        this.x = x;
        this.y = y;
        this.z = z;
        botsInRange = 0;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getZ() {
        return z;
    }

    public int getBotsInRange() {
        return botsInRange;
    }

    public void incBotsInRange() {
        botsInRange++;
    }

    public void setBotsInRange(int botsInRange) {
        this.botsInRange = botsInRange;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Point point = (Point) o;
        return x == point.x &&
                y == point.y &&
                z == point.z;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, z);
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ", " + z + ")";
    }
}
