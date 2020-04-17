package aoc.days.day15;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.List;

public class Day15 extends Day {
    private List<Disc> discs1;
    private List<Disc> discs2;

    public static void main(String[] args) {
        new Day15();
    }

    @Override
    protected void part1() {
        System.out.println(getOptimalTime(discs1));
    }

    private int getOptimalTime(List<Disc> discs) {
        int maxIndex = 0;
        for (int i = 0; i < discs.size(); i++) {
            Disc d = discs.get(i);
            if (d.getnPositions() > discs.get(maxIndex).getnPositions()) {
                maxIndex = i;
            }
        }
        Disc d = discs.get(maxIndex);
        int period = d.getnPositions();
        int goalIndex = period - d.getId();
        if (goalIndex < 0) {
            goalIndex += period;
        }
        int time = 0;
        if (d.getPosition() < goalIndex) {
            time += goalIndex - d.getPosition();
        } else {
            time += goalIndex + (period - d.getPosition());
        }
        moveDiscs(discs, time);
        while (!done(discs)) {
            time += period;
            moveDiscs(discs, period);
            period = updatePeriod(discs);
        }
        return time;
    }

    private int updatePeriod(List<Disc> discs) {
        int period = 1;
        for (Disc d : discs) {
            if (d.isDone()) {
                period *= d.getnPositions();
            }
        }
        return period;
    }

    private boolean done(List<Disc> discs) {
        for (Disc d : discs) {
            if (!d.isDone()) {
                return false;
            }
        }
        return true;
    }

    private void moveDiscs(List<Disc> discs, int time) {
        for (Disc d : discs) {
            d.move(time);
        }
    }

    @Override
    protected void part2() {
        System.out.println(getOptimalTime(discs2));
    }

    @Override
    protected void setup() {
        discs1 = new ArrayList<>();
        discs2 = new ArrayList<>();
        for (String s : lines) {
            List<Integer> ints = InputUtils.getInts(s);
            discs1.add(new Disc(ints.get(0), ints.get(1), ints.get(3)));
            discs2.add(new Disc(ints.get(0), ints.get(1), ints.get(3)));
        }
        discs2.add(new Disc(lines.size() + 1, 11, 0));
    }
}
