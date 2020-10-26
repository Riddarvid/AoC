package aoc.days.Day20;

import java.util.Comparator;

public class CompDijkstraShort implements Comparator<Point> {
    @Override
    public int compare(Point o1, Point o2) {
        return o1.distance - o2.distance;
    }
}
