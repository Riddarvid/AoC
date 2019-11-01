package aoc.days.day1;

import aoc.days.Day;

public class Day1 extends Day {
    private String input;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (int i = 0; i < input.length() - 1; i++) {
            if (input.charAt(i) == input.charAt(i + 1)) {
                sum += Character.getNumericValue(input.charAt(i));
            }
        }
        if (input.charAt(0) == input.charAt(input.length() - 1)) {
            sum += Character.getNumericValue(input.charAt(0));
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        int sum = 0;
        int offset = input.length() / 2;
        for (int i = 0; i < input.length(); i++) {
            if (input.charAt(i) == input.charAt((i + offset) % input.length())) {
                sum += Character.getNumericValue(input.charAt(i));
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void setup() {
        input = lines.get(0);
    }
}
