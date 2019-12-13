package aoc.days.day12;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day12 extends Day {
    private List<Moon> moons1;
    private List<Moon> moons2;

    public static void main(String[] args) {
        new Day12();
    }

    @Override
    protected void part1() {
        for (int i = 0; i < 1000; i++) {
            step(moons1);
        }
        System.out.println(getTotalEnergy());
    }

    private long getTotalEnergy() {
        long total = 0;
        for (Moon moon : moons1) {
            total += moon.getTotalEnergy();
        }
        return total;
    }

    private void step(List<Moon> moons) {
        for (Moon moon : moons) {
            moon.updateVelocity(moons);
        }
        for (Moon moon : moons) {
            moon.updatePosition();
        }
    }

    @Override
    protected void part2() {
        Set<State> states = new HashSet<>();
        states.add(new State(moons2));
        int i = 0;
        while (true) {
            i++;
            step(moons2);
            State next = new State(moons2);
            if (states.contains(next)) {
                break;
            } else {
                states.add(next);
            }
        }
        System.out.println(i);
    }

    @Override
    protected void setup() {
        moons1 = new ArrayList<>();
        moons2 = new ArrayList<>();
        for (String s : lines) {
            List<Long> coords = InputUtils.getIntsNegative(s);
            long x = coords.get(0);
            long y = coords.get(1);
            long z = coords.get(2);
            moons1.add(new Moon(x, y, z));
            moons2.add(new Moon(x, y, z));
        }
    }
}
