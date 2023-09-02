package aoc15.days.day1;

import aoc.days.Day;

public class Day1 extends Day {
    private char[] input;

    @Override
    public long part1() {
        int floor = 0;
        for (char c : input) {
            if (c == '(') {
                floor++;
            } else {
                floor--;
            }
        }
        return floor;
    }

    @Override
    public long part2() {
        int floor = 0;
        for (int i = 0; i < input.length; i++) {
            if (input[i] == '(') {
                floor++;
            } else {
                floor--;
            }
            if (floor == -1) {
                return i + 1;
            }
        }
        throw new IllegalArgumentException("Santa never enters the basement.");
    }

    @Override
    public void setup() {
        input = lines.get(0).toCharArray();
    }
}
