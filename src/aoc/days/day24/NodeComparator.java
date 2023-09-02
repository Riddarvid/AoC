package aoc.days.day24;

import aoc.utils.geometry.Point2D;

import java.util.Comparator;
import java.util.Map;

public class NodeComparator implements Comparator<Point2D> {
    private final Map<Point2D, Integer> distances;

    public NodeComparator(Map<Point2D, Integer> distances) {
        this.distances = distances;
    }

    @Override
    public int compare(Point2D p1, Point2D p2) {
        int d1 = distances.getOrDefault(p1, Integer.MAX_VALUE);
        int d2 = distances.getOrDefault(p2, Integer.MAX_VALUE);
        return d1 - d2;
    }
}
