package aoc.days.Day23;

import java.util.Comparator;

public class CompZ implements Comparator<NanoBot> {
    @Override
    public int compare(NanoBot o1, NanoBot o2) {
        return o1.getPoint().getZ() - o2.getPoint().getZ();
    }
}
