package AoC.Day23;

import java.util.Comparator;

public class CompY implements Comparator<NanoBot> {
    @Override
    public int compare(NanoBot o1, NanoBot o2) {
        return o1.getPoint().getY() - o2.getPoint().getY();
    }
}
