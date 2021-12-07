package aoc19.days.day20;

import aoc19.utils.geometry.Point2D;

import java.util.InputMismatchException;
import java.util.Objects;

public class Location {
    private final Point2D point;
    private final int depth;

    public Location(Point2D point, int depth) {
        if (depth < 0) {
            throw new InputMismatchException();
        }
        this.point = point;
        this.depth = depth;
    }

    public Point2D getPoint() {
        return point;
    }

    public int getDepth() {
        return depth;
    }

    public long getX() {
        return point.getX();
    }

    public long getY() {
        return point.getY();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Location location = (Location) o;
        return depth == location.depth &&
                Objects.equals(point, location.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point, depth);
    }

    @Override
    public String toString() {
        return point.toString() + " Depth: " + depth;
    }
}
