package aoc21.days.day15;

import aoc.math.geometry.Point;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class Node implements Comparable<Node> {
    private final Point position;
    private final long riskLevel;
    private final Set<Node> neighbours;

    private long totalRiskLevel;
    private Node previous;

    public Node(Point position, long riskLevel) {
        this.position = position;
        this.riskLevel = riskLevel;
        neighbours = new HashSet<>();
        totalRiskLevel = Integer.MAX_VALUE;
    }

    public void addNeighbour(Node neighbour) {
        neighbours.add(neighbour);
    }

    public Set<Node> getNeighbours() {
        return neighbours;
    }

    public long getRiskLevel() {
        return riskLevel;
    }

    public long getTotalRiskLevel() {
        return totalRiskLevel;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Node node = (Node) o;
        return riskLevel == node.riskLevel && Objects.equals(position, node.position);
    }

    @Override
    public int hashCode() {
        return Objects.hash(position, riskLevel);
    }

    @Override
    public String toString() {
        return position.toString() + " " + riskLevel;
    }

    @Override
    public int compareTo(Node other) {
        if (this.totalRiskLevel > other.totalRiskLevel) {
            return 1;
        } else if (this.totalRiskLevel < other.totalRiskLevel) {
            return -1;
        }
        return 0;
    }

    public void setTotalRiskLevel(long value) {
        totalRiskLevel = value;
    }

    public void updateTotalRiskLevel(long newRiskLevel, Node previous) {
        if (newRiskLevel < totalRiskLevel) {
            totalRiskLevel = newRiskLevel;
            this.previous = previous;
        }
    }
}
