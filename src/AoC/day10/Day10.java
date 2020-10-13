package AoC.day10;

import riddarvid.aoc.days.Day;

public class Day10 extends Day {
    public static void main(String[] args) {
        new Day10();
    }

    private String startSequence;

    @Override
    protected void part1() {
        String previous = startSequence;
        for (int i = 0; i < 40; i++) {
            previous = lookAndSay(previous);
        }
        System.out.println(previous.length());
    }

    private String lookAndSay(String previous) {
        StringBuilder sb = new StringBuilder();
        char[] chars = previous.toCharArray();
        char current = chars[0];
        int i = 1;
        int count = 1;
        while (i < chars.length) {
            while (i < chars.length && current == chars[i]) {
                i++;
                count++;
            }
            sb.append(count).append(current);
            if (i >= chars.length) {
                break;
            }
            current = chars[i];
            count = 0;
        }
        return sb.toString();
    }

    @Override
    protected void part2() {
        String previous = startSequence;
        for (int i = 0; i < 50; i++) {
            previous = lookAndSay(previous);
        }
        System.out.println(previous.length());
    }

    @Override
    protected void setup() {
        startSequence = lines.get(0);
    }
}
