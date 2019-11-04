package aoc.days.day10;

import aoc.FileUtilities;
import aoc.days.Day;
import aoc.days.KnotHash;

import java.util.ArrayList;
import java.util.List;

public class Day10 extends Day {
    private List<Integer> lengths;
    private int[] knot1;
    private String key;
    public static void main(String[] args) {
        new Day10();
    }

    @Override
    protected void part1() {
        int current = 0;
        int skipSize = 0;
        for (int length : lengths) {
            reverse(knot1, current, length);
            current += length + skipSize;
            skipSize++;
        }
        System.out.println(knot1[0] * knot1[1]);
    }

    private void reverse(int[] knot, int current, int length) {
        int startIndex = current;
        int endIndex = current + length - 1;
        while (endIndex > startIndex) {
            int temp = knot[endIndex % knot.length];
            knot[endIndex % knot.length] = knot[startIndex % knot.length];
            knot[startIndex % knot.length] = temp;
            endIndex--;
            startIndex++;
        }
    }

    @Override
    protected void part2() {
        System.out.println(KnotHash.hash(key));
    }

    @Override
    protected void setup() {
        lengths = new ArrayList<>();
        for (String s : FileUtilities.getTokens(lines.get(0), ',')) {
            lengths.add(Integer.parseInt(s));
        }
        StringBuilder sb = new StringBuilder();
        for (char c : lines.get(0).toCharArray()) {
            sb.append(c);
        }
        key = sb.toString();
        knot1 = new int[256];
        for (int i = 1; i < knot1.length; i++) {
            knot1[i] = i;
        }
    }
}
