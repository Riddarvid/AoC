package AoC.Day23;

import java.util.Comparator;

public class CompX implements Comparator<NanoBot> {
    @Override
    public int compare(NanoBot o1, NanoBot o2) {
        return o1.getPoint().getX() - o2.getPoint().getX();
    }
}
