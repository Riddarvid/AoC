package aoc.days.day18;

import aoc.utils.geometry.Point2D;

import java.util.Objects;

public class DijkstraPoint {
    private final Point2D point;
    private DijkstraPoint previous;
    private boolean visited;

    public DijkstraPoint(Point2D point) {
        this.point = point;
        visited = false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DijkstraPoint that = (DijkstraPoint) o;
        return Objects.equals(point, that.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point);
    }

    @Override
    public String toString() {
        return point.toString();
    }

    public Point2D getPoint() {
        return point;
    }

    public DijkstraPoint getPrevious() {
        return previous;
    }

    public void setPrevious(DijkstraPoint previous) {
        this.previous = previous;
    }

    public boolean isVisited() {
        return visited;
    }

    public void setVisited(boolean visited) {
        this.visited = visited;
    }
}
