package aoc19.days.day18;

import java.util.Objects;

public class Edge {
    private final Node start;
    private final Node end;
    private final int length;

    public Edge(Node start, Node end, int length) {
        this.start = start;
        this.end = end;
        this.length = length;
    }

    public Node getStart() {
        return start;
    }

    public Node getEnd() {
        return end;
    }

    public int getLength() {
        return length;
    }

    @Override
    public String toString() {
        return start.toString() + " -> " + end.toString() + " distance: " + length;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Edge edge = (Edge) o;
        return length == edge.length &&
                Objects.equals(start, edge.start) &&
                Objects.equals(end, edge.end);
    }

    @Override
    public int hashCode() {
        return Objects.hash(start, end, length);
    }
}
