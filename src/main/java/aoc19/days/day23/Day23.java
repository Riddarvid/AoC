package aoc19.days.day23;

import aoc19.utils.input.InputUtils;
import aoc.days.Day;

public class Day23 extends Day {
    private long[] memory;

    public static void main(String[] args) {
        new Day23();
    }

    @Override
    public long part1() {
        //Network network = new Network(50, memory, true);
        //network.run();
    }

    @Override
    public long part2() {
        Network network = new Network(50, memory);
        network.run();
        return 0;
    }

    @Override
    public void setup() {
        memory = InputUtils.readProgram(lines);
    }
}
