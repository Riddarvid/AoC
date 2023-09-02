package aoc.days.day24;

import aoc.utils.geometry.Point2D;

import java.util.Objects;

public class Edge implements Comparable<Edge> {
    private final Point2D start;
    private final Point2D end;
    private final int distance;

    public Edge(Point2D start, Point2D end, int distance) {
        this.start = start;
        this.end = end;
        this.distance = distance;
    }

    public Point2D getStart() {
        return start;
    }

    public Point2D getEnd() {
        return end;
    }

    public int getDistance() {
        return distance;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Edge edge = (Edge) o;
        return Objects.equals(start, edge.start) &&
                Objects.equals(end, edge.end);
    }

    @Override
    public int hashCode() {
        return Objects.hash(start, end);
    }

    @Override
    public int compareTo(Edge edge) {
        return this.getDistance() - edge.getDistance();
    }
}
