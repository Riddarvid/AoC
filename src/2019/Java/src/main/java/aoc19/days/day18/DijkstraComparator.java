package aoc19.days.day18;

import java.util.Comparator;
import java.util.Map;

public class DijkstraComparator implements Comparator<DijkstraNode> {
    private final Map<DijkstraNode, Integer> distances;

    public DijkstraComparator(Map<DijkstraNode, Integer> distances) {
        this.distances = distances;
    }

    @Override
    public int compare(DijkstraNode n1, DijkstraNode n2) {
        int distance1 = distances.get(n1);
        int distance2 = distances.get(n2);
        return distance1 - distance2;
    }
}
