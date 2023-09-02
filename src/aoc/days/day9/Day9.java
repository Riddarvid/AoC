package aoc.days.day9;

import aoc.days.Day;

public class Day9 extends Day {
    private Group root;

    public static void main(String[] args) {
        new Day9();
    }

    @Override
    protected void part1() {
        System.out.println(root.getScore(1));
    }

    @Override
    protected void part2() {
        System.out.println(Group.getGarbageAmount());
    }

    @Override
    protected void setup() {
        root = new Group();root.init(lines.get(0), 1);
    }
}
