package aoc.days.Day4;

import java.util.List;

public class Shift {
    private final PointInTime start;
    private final PointInTime end;
    private boolean[] awake = new boolean[60];

    public Shift(PointInTime start, PointInTime end, List<Integer> sleepWake) {
        this.start = start;
        this.end = end;
        int pos = 0;
        int time;
        if (!sleepWake.isEmpty()) {
            while (!sleepWake.isEmpty()) {

            }
        } else {
            while (pos < awake.length) {
                awake[pos] = true;
            }
        }
    }
}
