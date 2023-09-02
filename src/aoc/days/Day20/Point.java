package aoc.days.Day20;

import java.util.Objects;

public class Point {
    int x;
    int y;
    Point north;
    Point east;
    Point south;
    Point west;
    //Dijkstra
    Point previous = null;
    int distance = 1000000000;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Point point = (Point) o;
        return x == point.x &&
                y == point.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
