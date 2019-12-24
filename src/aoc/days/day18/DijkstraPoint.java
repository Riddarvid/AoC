package aoc.days.day18;

import aoc.utils.geometry.Point2D;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class DijkstraPoint {
    private final Point2D point;
    private final Set<Point2D> keys;

    public DijkstraPoint(Point2D point, Set<Point2D> keys) {
        this.keys = new HashSet<>(keys);
        this.point = point;
    }

    public boolean hasKey(Point2D key) {
        return keys.contains(key);
    }

    public Point2D getPoint() {
        return new Point2D(point);
    }

    public int nKeys() {
        return keys.size();
    }

    public Set<Point2D> getKeys() {
        return new HashSet<>(keys);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DijkstraPoint that = (DijkstraPoint) o;
        return Objects.equals(point, that.point) &&
                Objects.equals(keys, that.keys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point, keys);
    }

    @Override
    public String toString() {
        if (keys.size() == 0) {
            return point.toString();
        }
        StringBuilder ksb = new StringBuilder();
        for (Point2D key : keys) {
            ksb.append(key.toString()).append(", ");
        }
        ksb.delete(ksb.length() - 2, ksb.length());
        return point.toString() + " {" + ksb.toString() + "}";
    }
}
