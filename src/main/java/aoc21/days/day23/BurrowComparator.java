package aoc21.days.day23;

import java.util.Comparator;
import java.util.Map;

public class BurrowComparator implements Comparator<Burrow> {
    private final Map<Burrow, Integer> costMap;

    public BurrowComparator(Map<Burrow, Integer> costMap) {
        this.costMap = costMap;
    }

    @Override
    public int compare(Burrow burrow1, Burrow burrow2) {
        //return burrow2.getNumberOfCorrect() - burrow1.getNumberOfCorrect();
        return costMap.getOrDefault(burrow1, Integer.MAX_VALUE) - costMap.getOrDefault(burrow2, Integer.MAX_VALUE);
    }
}
