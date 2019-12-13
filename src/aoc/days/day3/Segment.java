package aoc.days.day3;

import aoc.utils.math.Point;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Segment {
    private final Point start;
    private final Point end;
    private final boolean horizontal;
    private final int length;

    public Segment(Point start, Point end) {
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

    private boolean contains(Point point) {
        if (horizontal) {
            int maxX = Math.max(start.getX(), end.getX());
            int minX = Math.min(start.getX(), end.getX());
            return point.getY() == start.getY() && point.getX() >= minX && point.getX() <= maxX;
        } else {
            int maxY = Math.max(start.getY(), end.getY());
            int minY = Math.min(start.getY(), end.getY());
            return point.getX() == start.getX() && point.getY() >= minY && point.getY() <= maxY;
        }
    }

    public Point getIntersection(Segment other) {
        if (horizontal && !other.horizontal) {
            Point potential = new Point(other.start.getX(), start.getY());
            if (!potential.equals(new Point(0, 0)) && contains(potential) && other.contains(potential)) {
                return potential;
            }
        } else {
            if (other.horizontal) {
                Point potential = new Point(start.getX(), other.start.getY());
                if (!potential.equals(new Point(0, 0)) && contains(potential) && other.contains(potential)) {
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

    public int length() {
        return length;
    }

    public int distance(Point point) {
        if (horizontal) {
            return Math.abs(point.getX() - start.getX()) + 1;
        } else {
            return Math.abs(point.getY() - start.getY()) + 1;
        }
    }
}
