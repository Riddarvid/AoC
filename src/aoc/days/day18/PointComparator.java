package aoc.days.day18;

import java.util.Comparator;
import java.util.Map;

public class PointComparator implements Comparator<DijkstraPoint> {
    private final Map<DijkstraPoint, Integer> dijkstraDistances;

    public PointComparator(Map<DijkstraPoint, Integer> dijkstraDistances) {
        this.dijkstraDistances = dijkstraDistances;
    }

    @Override
    public int compare(DijkstraPoint p1, DijkstraPoint p2) {
        Integer d1, d2;
        d1 = dijkstraDistances.get(p1);
        d2 = dijkstraDistances.get(p2);
        if (d1 == null) {
            d1 = Integer.MAX_VALUE;
        }
        if (d2 == null) {
            d2 = Integer.MAX_VALUE;
        }
        return d1 - d2;
    }
}
