package aoc15.days.day23;

import aoc.days.Day;

public class Day23 extends Day {

    private int collatz(int a) {
        int b = 0;
        while (a > 1) {
            b++;
            if (a % 2 == 0) {
                a = a / 2;
            } else {
                a = 3 * a + 1;
            }
        }
        return b;
    }

    @Override
    public long part1() {
        return collatz(20895);
    }

    @Override
    public long part2() {
        return collatz(60975);
    }

    @Override
    public void setup() {

    }
}
