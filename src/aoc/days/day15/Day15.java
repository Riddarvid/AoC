package aoc.days.day15;

import riddarvid.aoc.days.Day;
import aoc.utils.input.InputUtils;

public class Day15 extends Day {
    private long[] memory;
    private Drone drone;

    public static void main(String[] args) {
        new Day15();
    }

    @Override
    protected void part1() {
        drone = new Drone();
        drone.createMap(memory);
        //drone.printMap();
        System.out.println(drone.findShortestPath());
    }

    @Override
    protected void part2() {
        System.out.println(drone.findOxygenTime());
    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}
