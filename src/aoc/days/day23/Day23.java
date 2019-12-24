package aoc.days.day23;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

public class Day23 extends Day {
    private long[] memory;

    public static void main(String[] args) {
        new Day23();
    }

    @Override
    protected void part1() {
        //Network network = new Network(50, memory, true);
        //network.run();
    }

    @Override
    protected void part2() {
        Network network = new Network(50, memory);
        network.run();
    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}
