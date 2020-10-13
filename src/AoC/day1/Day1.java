package AoC.day1;

import riddarvid.aoc.days.Day;

public class Day1 extends Day {
    private char[] input;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    public void part1() {
        int floor = 0;
        for (char c : input) {
            if (c == '(') {
                floor++;
            }
        }
        floor = 2 * floor - input.length;
        System.out.println(floor);
    }

    @Override
    public void part2() {
        int floor = 0;
        for (int i = 0; i < input.length; i++) {
            if (input[i] == '(') {
                floor++;
            } else {
                floor--;
            }
            if (floor == -1) {
                System.out.println(i + 1);
                break;
            }
        }
    }

    @Override
    protected void setup() {
        input = lines.get(0).toCharArray();
    }
}
