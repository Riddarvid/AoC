package aoc.days.day19;

import aoc.days.Day;

public class Day19 extends Day {
    private int nElves;

    public static void main(String[] args) {
        new Day19();
    }

    @Override
    protected void part1() {
        Circle circle = new Circle(nElves);
        while (circle.getSize() > 1) {
            circle.steal();
        }
        System.out.println(circle.getCurrent());
    }

    @Override
    protected void part2() {
        Circle2 circle = new Circle2(nElves);
        while (circle.getSize() > 1) {
            circle.steal();
        }
        System.out.println(circle.getCurrent());
    }

    @Override
    protected void setup() {
        nElves = Integer.parseInt(lines.get(0));
    }
}
