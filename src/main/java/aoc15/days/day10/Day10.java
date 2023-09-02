package aoc15.days.day10;

import aoc.days.Day;

public class Day10 extends Day {

    private String startSequence;

    @Override
    public long part1() {
        String previous = startSequence;
        for (int i = 0; i < 40; i++) {
            previous = lookAndSay(previous);
        }
        return previous.length();
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
    public long part2() {
        String previous = startSequence;
        for (int i = 0; i < 50; i++) {
            previous = lookAndSay(previous);
        }
        return previous.length();
    }

    @Override
    public void setup() {
        startSequence = lines.get(0);
    }
}
