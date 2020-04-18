package aoc.days.day19;

import aoc.days.Day;

import java.util.LinkedList;
import java.util.List;

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
        System.out.println(circle.getCurrent().getPresents());
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        nElves = Integer.parseInt(lines.get(0));
    }
}
