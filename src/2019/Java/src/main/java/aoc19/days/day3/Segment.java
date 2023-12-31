package aoc19.days.day3;

import aoc19.utils.geometry.Point2D;

import java.util.InputMismatchException;

public class Segment {
    private final Point2D start;
    private final Point2D end;
    private final boolean horizontal;
    private final long length;

    public Segment(Point2D start, Point2D end) {
        if (start.getX() != end.getX() && start.getY() != end.getY()) {
            throw new InputMismatchException("Points not on a line");
        }
        if (start.getY() == end.getY()) {
            horizontal = true;
            length = Math.abs(start.getX() - end.getX()) + 1;
        } else {
            horizontal = false;
            length = Math.abs(start.getY() - end.getY()) + 1;
        }
        this.start = start;
        this.end = end;
    }

    private boolean contains(Point2D point) {
        if (horizontal) {
            long maxX = Math.max(start.getX(), end.getX());
            long minX = Math.min(start.getX(), end.getX());
            return point.getY() == start.getY() && point.getX() >= minX && point.getX() <= maxX;
        } else {
            long maxY = Math.max(start.getY(), end.getY());
            long minY = Math.min(start.getY(), end.getY());
            return point.getX() == start.getX() && point.getY() >= minY && point.getY() <= maxY;
        }
    }

    public Point2D getIntersection(Segment other) {
        if (horizontal && !other.horizontal) {
            Point2D potential = new Point2D(other.start.getX(), start.getY());
            if (!potential.equals(new Point2D(0, 0)) && contains(potential) && other.contains(potential)) {
                return potential;
            }
        } else {
            if (other.horizontal) {
                Point2D potential = new Point2D(start.getX(), other.start.getY());
                if (!potential.equals(new Point2D(0, 0)) && contains(potential) && other.contains(potential)) {
                    return potential;
                }
            }
        }
        return null;
    }

    @Override
    public String toString() {
        return start + " -> " + end;
    }

    public long length() {
        return length;
    }

    public long distance(Point2D point) {
        if (horizontal) {
            return Math.abs(point.getX() - start.getX()) + 1;
        } else {
            return Math.abs(point.getY() - start.getY()) + 1;
        }
    }
}
