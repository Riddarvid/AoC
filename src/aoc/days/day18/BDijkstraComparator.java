package aoc.days.day18;

import java.util.Comparator;
import java.util.Map;

public class BDijkstraComparator implements Comparator<BDijkstraNode> {
    private final Map<BDijkstraNode, Integer> distances;

    public BDijkstraComparator(Map<BDijkstraNode, Integer> distances) {
        this.distances = distances;
    }

    @Override
    public int compare(BDijkstraNode n1, BDijkstraNode n2) {
        int distance1 = distances.get(n1);
        int distance2 = distances.get(n2);
        return distance1 - distance2;
    }
}
