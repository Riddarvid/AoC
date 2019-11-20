package AoC.Day23;

import java.util.Comparator;

public class CompBotRange implements Comparator<NanoBot> {
    @Override
    public int compare(NanoBot o1, NanoBot o2) {
        return (int)(o2.getRadius() - o1.getRadius());
    }
}
