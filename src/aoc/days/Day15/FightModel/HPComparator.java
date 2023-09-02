package aoc.days.Day15.FightModel;

import java.util.Comparator;

public class HPComparator implements Comparator<Unit> {
    @Override
    public int compare(Unit o1, Unit o2) {
        return o1.getHealth() - o2.getHealth();
    }
}
