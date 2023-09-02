package aoc15.days.day6;

import aoc.days.Day;
import aoc.math.geometry.Point;
import aoc.parsing.ParsingUtils;

import java.util.List;

public class Day6 extends Day {
    private enum Operation {
        TURN_ON, TURN_OFF, TOGGLE
    }

    private boolean[][] lights1;
    private int[][] lights2;

    @Override
    public long part1() {
        for (String instruction : lines) {
            Operation operation;
            if (instruction.charAt(1) == 'o') {
                operation = Operation.TOGGLE;
            } else if (instruction.charAt(6) == 'f') {
                operation = Operation.TURN_OFF;
            } else {
                operation = Operation.TURN_ON;
            }
            List<Integer> integers = ParsingUtils.getIntegers(instruction);
            Point p1 = new Point(integers.get(0), integers.get(1));
            Point p2 = new Point(integers.get(2), integers.get(3));
            operateLights1(operation, p1, p2);
        }
        return getNumberOfLights();
    }

    @Override
    public long part2() {
        for (String instruction : lines) {
            Operation operation;
            if (instruction.charAt(1) == 'o') {
                operation = Operation.TOGGLE;
            } else if (instruction.charAt(6) == 'f') {
                operation = Operation.TURN_OFF;
            } else {
                operation = Operation.TURN_ON;
            }
            List<Integer> integers = ParsingUtils.getIntegers(instruction);
            Point p1 = new Point(integers.get(0), integers.get(1));
            Point p2 = new Point(integers.get(2), integers.get(3));
            operateLights2(operation, p1, p2);
        }
        return getBrightness();
    }

    @Override
    public void setup() {
        lights1 = new boolean[1000][1000];
        lights2 = new int[1000][1000];
    }

    private int getBrightness() {
        int brightness = 0;
        for (int row = 0; row < 1000; row++) {
            for (int col = 0; col < 1000; col++) {
                brightness += lights2[row][col];
            }
        }
        return brightness;
    }

    private void operateLights2(Operation operation, Point p1, Point p2) {
        int minRow;
        int maxRow;
        int minCol;
        int maxCol;
        if (p1.getY() > p2.getY()) {
            minRow = p2.getY();
            maxRow = p1.getY();
        } else {
            minRow = p1.getY();
            maxRow = p2.getY();
        }
        if (p1.getX() > p2.getX()) {
            minCol = p2.getX();
            maxCol = p1.getX();
        } else {
            minCol = p1.getX();
            maxCol = p2.getX();
        }
        switch (operation) {
            case TURN_ON:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        lights2[row][col] += 1;
                    }
                }
                break;
            case TURN_OFF:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        if (lights2[row][col] > 0) {
                            lights2[row][col] -= 1;
                        }
                    }
                }
                break;
            case TOGGLE:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        lights2[row][col] += 2;
                    }
                }
                break;
        }
    }

    private int getNumberOfLights() {
        int count = 0;
        for (int row = 0; row < 1000; row++) {
            for (int col = 0; col < 1000; col++) {
                if (lights1[row][col]) {
                    count++;
                }
            }
        }
        return count;
    }

    private void operateLights1(Operation operation, Point p1, Point p2) {
        int minRow;
        int maxRow;
        int minCol;
        int maxCol;
        if (p1.getY() > p2.getY()) {
            minRow = p2.getY();
            maxRow = p1.getY();
        } else {
            minRow = p1.getY();
            maxRow = p2.getY();
        }
        if (p1.getX() > p2.getX()) {
            minCol = p2.getX();
            maxCol = p1.getX();
        } else {
            minCol = p1.getX();
            maxCol = p2.getX();
        }
        switch (operation) {
            case TURN_ON:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        lights1[row][col] = true;
                    }
                }
                break;
            case TURN_OFF:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        lights1[row][col] = false;
                    }
                }
                break;
            case TOGGLE:
                for (int row = minRow; row <= maxRow; row++) {
                    for (int col = minCol; col <= maxCol; col++) {
                        lights1[row][col] = !lights1[row][col];
                    }
                }
                break;
        }
    }
}
