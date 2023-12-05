package aoc.days.Day20;

import java.util.Comparator;

public class CompDijkstraLong implements Comparator<Point> {
    @Override
    public int compare(Point o1, Point o2) {
        return o2.distance - o1.distance;
    }
}
