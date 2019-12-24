package aoc.days.day18;

import java.util.Comparator;
import java.util.Map;

public class DijkstraComparator implements Comparator<DijkstraPoint> {
    private final Map<DijkstraPoint, Integer> distances;

    public DijkstraComparator(Map<DijkstraPoint, Integer> distances) {
        this.distances = distances;
    }

    @Override
    public int compare(DijkstraPoint a, DijkstraPoint b) {
        return distances.get(a) - distances.get(b);
    }
}
