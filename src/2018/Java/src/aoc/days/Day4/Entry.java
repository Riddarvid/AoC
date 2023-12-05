package aoc.days.Day4;

import aoc.Action;

public class Entry implements Comparable<Entry> {
    private PointInTime time;
    private Action action;
    private int id;

    public Entry(PointInTime time, Action action, int id) {
        this.time = time;
        this.action = action;
        this.id = id;
    }

    @Override
    public int compareTo(Entry o) {
        return this.time.compareTo(o.time);
    }

    public PointInTime getTime() {
        return time;
    }

    public Action getAction() {
        return action;
    }

    public int getId() {
        return id;
    }
}
