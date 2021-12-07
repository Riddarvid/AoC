package days.day7;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
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

    private int getMeanPosition(List<Integer> positions) {
        long sum = 0;
        for (int position : positions) {
            sum += position;
        }
        return (int) (sum / positions.size());
    }

    private long getError(List<Integer> positions, int goal) {
        long error = 0;
        for (int position : positions) {
            error += Math.abs(position - goal);
        }
        return error;
    }

    private long getError2(List<Integer> positions, int goal) {
        long error = 0;
        for (int position : positions) {
            error += getFuel2(position, goal);
        }
        return error;
    }

    private long getFuel2(int position, int goal) {
        long distance = Math.abs(position - goal);
        return distance * (distance + 1) / 2;
    }

    @Override
    public long part1() {
        long minError = Integer.MAX_VALUE;
        for (int position = minPosition; position <= maxPosition; position++) {
            long error = getError(horizontalPositions, position);
            if (error < minError) {
                minError = error;
            }
        }
        return minError;
    }

    @Override
    public long part2() {
        long minError = Integer.MAX_VALUE;
        for (int position = minPosition; position <= maxPosition; position++) {
            long error = getError2(horizontalPositions, position);
            if (error < minError) {
                minError = error;
            }
        }
        return minError;
    }

    @Override
    public void setup() {
        horizontalPositions = ParsingUtils.getIntegers(lines.get(0));
        maxPosition = 0;
        minPosition = Integer.MAX_VALUE;
        for (int value : horizontalPositions) {
            if (value > maxPosition) {
                maxPosition = value;
            }
            if (value < minPosition) {
                minPosition = value;
            }
        }
    }
}
