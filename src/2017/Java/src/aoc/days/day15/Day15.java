package aoc.days.day15;

import aoc.days.Day;
import aoc.utils.InputUtilities;

public class Day15 extends Day {
    private Generator genA;
    private Generator genB;

    public static void main(String[] args) {
        new Day15();
    }

    @Override
    protected void part1() {
        int count = 0;
        for (int i = 0; i < 40_000_000; i++) {
            genA.generateNext();
            genB.generateNext();
            if (close(genA.getLast(), genB.getLast())) {
                count++;
            }
        }
        System.out.println(count);
    }

    private boolean close(long a, long b) {
        return a % Math.pow(2, 16) == b % Math.pow(2, 16);
    }

    @Override
    protected void part2() {
        genA.reset();
        genA.setStrategy(new Advanced());
        genB.reset();
        genB.setStrategy(new Advanced());
        int count = 0;
        for (int i = 0; i < 5_000_000; i++) {
            genA.generateNext();
            genB.generateNext();
            if (close(genA.getLast(), genB.getLast())) {
                count++;
            }
        }
        System.out.println(count);
    }

    @Override
    protected void setup() {
        genA = new Generator(InputUtilities.getInts(lines.get(0)).get(0), 16807, 4);
        genB = new Generator(InputUtilities.getInts(lines.get(1)).get(0), 48271, 8);
    }
}
