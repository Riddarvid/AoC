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

    private double getMedian(List<Integer> positions) {
        positions.sort(Integer::compareTo);
        if (positions.size() % 2 == 1) {
            return positions.get(positions.size() / 2);
        } else {
            double a = positions.get(positions.size() / 2);
            double b = positions.get(positions.size() / 2 - 1);
            return (a + b) / 2;
        }
    }

    private double getMean(List<Integer> positions) {
        double sum = 0;
        for (int position : positions) {
            sum += position;
        }
        return sum / positions.size();
    }

    private int getLeastFuelPosition() {
        double sum = 0;
        for (int position : horizontalPositions) {
            sum += position;
        }
        double mean = sum / horizontalPositions.size();
        return (int) Math.floor(sum / horizontalPositions.size());
    }

    private long getLeastFuel(int minPosition, int maxPosition, FuelFunction fuelFunction) {
        long minFuel = Integer.MAX_VALUE;
        for (int position = minPosition; position <= maxPosition; position++) {
            long fuel = getTotalFuel(horizontalPositions, position, fuelFunction);
            if (fuel < minFuel) {
                minFuel = fuel;
            }
        }
        return minFuel;
    }

    private long getTotalFuel(List<Integer> positions, int goal, FuelFunction fuelFunction) {
        long fuel = 0;
        for (int position : positions) {
            fuel += fuelFunction.getFuel(position, goal);
        }
        return fuel;
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
        //System.out.println(getTotalFuel(horizontalPositions, (int) getMedian(horizontalPositions), this::getFuel1));
        return getLeastFuel(minPosition, maxPosition, this::getFuel1);
    }

    @Override
    public long part2() {
        //System.out.println(getTotalFuel(horizontalPositions, (int) (getMean(horizontalPositions) - 0.5), this::getFuel2));
        return getLeastFuel(minPosition, maxPosition, this::getFuel2);
    }

    @Override
    public void setup() {
        horizontalPositions = ParsingUtils.getIntegers(lines.get(0));
        maxPosition = Collections.max(horizontalPositions);
        minPosition = Collections.min(horizontalPositions);
    }
}
