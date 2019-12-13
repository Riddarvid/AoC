package aoc.days.day3;

import aoc.utils.math.Point;

import java.util.Objects;

public class Intersection {
    private final Point point;
    private final int steps;

    public Intersection(Point point, int steps) {
        this.point = point;
        this.steps = steps;
    }

    public Point getPoint() {
        return point;
    }

    public int getSteps() {
        return steps;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Intersection that = (Intersection) o;
        return Objects.equals(point, that.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point);
    }
}
