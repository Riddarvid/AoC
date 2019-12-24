package aoc.utils.geometry;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class Point2D {
    private final long x;
    private final long y;

    public Point2D(long x, long y) {
        this.x = x;
        this.y = y;
    }

    public Point2D(Point2D point) {
        this(point.x, point.y);
    }

    public Point2D(Point3D point) {
        x = point.getX();
        y = point.getY();
    }

    public long getX() {
        return x;
    }

    public long getY() {
        return y;
    }

    public long distanceFrom(Point2D other) {
        long deltaX = Math.abs(x - other.x);
        long deltaY = Math.abs(y - other.y);
        return deltaX + deltaY;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Point2D point = (Point2D) o;
        return x == point.x &&
                y == point.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }

    public Point2D moveBy(long x, long y) {
        return new Point2D(this.x + x, this.y + y);
    }

    public Point2D moveBy(Vector2D vector) {
        return moveBy(vector.getX(), vector.getY());
    }

    public Vector2D vectorTo(Point2D other) {
        return new Vector2D(other.x - x, other.y - y);
    }

    public Set<Point2D> neighbours() {
        Set<Point2D> neighbours = new HashSet<>();
        neighbours.add(moveBy(0, -1));
        neighbours.add(moveBy(0, 1));
        neighbours.add(moveBy(-1, 0));
        neighbours.add(moveBy(1, 0));
        return neighbours;
    }
}
