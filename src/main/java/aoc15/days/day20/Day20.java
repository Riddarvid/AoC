package aoc15.days.day20;

import aoc.days.Day;

public class Day20 extends Day {
    private int min;

    @Override
    public long part1() {
        int houseNumber = 700000;
        while (presentsToHouse1(houseNumber) < min) {
            houseNumber++;
        }
        return houseNumber;
    }

    private int presentsToHouse1(int houseNumber) {
        int presents = 0;
        for (int i = 1; i <= houseNumber / 2; i++) {
            if (houseNumber % i == 0) {
                presents += i;
            }
        }
        presents += houseNumber;
        return presents * 10;
    }

    @Override
    public long part2() {
        int houseNumber = 800000;
        while (presentsToHouse2(houseNumber) < min) {
            houseNumber++;
        }
        return houseNumber;
    }

    private int presentsToHouse2(int houseNumber) {
        int presents = 0;
        for (int i = (int) Math.ceil(houseNumber / 50.0); i <= houseNumber / 2; i++) {
            if (houseNumber % i == 0) {
                presents += i;
            }
        }
        presents += houseNumber;
        return presents * 11;
    }

    @Override
    public void setup() {
        min = Integer.parseInt(lines.get(0));
    }
}
