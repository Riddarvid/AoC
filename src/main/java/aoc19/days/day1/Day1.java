package aoc19.days.day1;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day1 extends Day {
    private List<Integer> masses;
    private int initFuel;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    public long part1() {
        initFuel = 0;
        for (int mass : masses) {
            initFuel += getFuel(mass);
        }
        return initFuel;
    }

    private int getFuel(int mass) {
        return mass / 3 - 2;
    }

    private int getFuelRecursive(int mass) {
        int totalFuel = 0;
        int remainingFuel = getFuel(mass);
        while (remainingFuel > 0) {
            totalFuel += remainingFuel;
            remainingFuel = getFuel(remainingFuel);
        }
        return totalFuel;
    }

    @Override
    public long part2() {
        initFuel = 0;
        for (int mass : masses) {
            initFuel += getFuelRecursive(mass);
        }
        return initFuel;
    }

    @Override
    public void setup() {
        masses = new ArrayList<>();
        for (String s : lines) {
            masses.add(Integer.parseInt(s));
        }
    }
}
