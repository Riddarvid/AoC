package days.day7;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.Collections;
import java.util.List;

public class Day7 extends Day {
    private List<Integer> horizontalPositions;
    private int minPosition;
    private int maxPosition;

    public static void main(String[] args) {
        new Day7().runAndPrint();
    }

    public Day7() {
        super("input7");
    }

    private long getLeastFuel(int minPosition, int maxPosition, FuelFunction fuelFunction) {
        long minError = Integer.MAX_VALUE;
        for (int position = minPosition; position <= maxPosition; position++) {
            long fuel = getTotalFuel(horizontalPositions, position, fuelFunction);
            if (fuel < minError) {
                minError = fuel;
            }
        }
        return minError;
    }

    private long getTotalFuel(List<Integer> positions, int goal, FuelFunction fuelFunction) {
        long error = 0;
        for (int position : positions) {
            error += fuelFunction.getFuel(position, goal);
        }
        return error;
    }

    private long getFuel1(int position, int goal) {
        return Math.abs(position - goal);
    }


    private long getFuel2(int position, int goal) {
        long distance = Math.abs(position - goal);
        return distance * (distance + 1) / 2;
    }

    @Override
    public long part1() {
        return getLeastFuel(minPosition, maxPosition, this::getFuel1);
    }

    @Override
    public long part2() {
        return getLeastFuel(minPosition, maxPosition, this::getFuel2);
    }

    @Override
    public void setup() {
        horizontalPositions = ParsingUtils.getIntegers(lines.get(0));
        maxPosition = Collections.max(horizontalPositions);
        minPosition = Collections.min(horizontalPositions);
    }
}
