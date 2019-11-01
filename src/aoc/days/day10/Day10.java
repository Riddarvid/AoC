package aoc.days.day10;

import aoc.FileUtilities;
import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day10 extends Day {
    private List<Integer> lengths;
    private List<Integer> lengthsAscii;
    private int[] knot1;
    private int[] knot2;
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
        int current = 0;
        int skipSize = 0;
        for (int i = 0; i < 64; i++) {
            for (int length : lengthsAscii) {
                reverse(knot2, current, length);
                current += length + skipSize;
                skipSize++;
            }
        }
        int[] denseHash = createDenseHash(knot2);
        System.out.println(toHex(denseHash));
    }

    private String toHex(int[] denseHash) {
        StringBuilder hex = new StringBuilder();
        for (int n : denseHash) {
            hex.append(String.format("%02x", n));
        }
        return hex.toString();
    }

    private int[] createDenseHash(int[] sparseHash) {
        int[] denseHash = new int[sparseHash.length / 16];
        for (int i = 0; i < 16; i++) {
            denseHash[i] = hash(knot2, i * 16);
        }
        return denseHash;
    }

    private int hash(int[] hash, int index) {
        int n = hash[index];
        for (int i = index + 1; i < index + 16; i++) {
            n ^= hash[i];
        }
        return n;
    }

    @Override
    protected void setup() {
        lengths = new ArrayList<>();
        for (String s : FileUtilities.getTokens(lines.get(0), ',')) {
            lengths.add(Integer.parseInt(s));
        }
        lengthsAscii = new ArrayList<>();
        for (char c : lines.get(0).toCharArray()) {
            lengthsAscii.add((int)c);
        }
        lengthsAscii.add(17);
        lengthsAscii.add(31);
        lengthsAscii.add(73);
        lengthsAscii.add(47);
        lengthsAscii.add(23);
        knot1 = new int[256];
        knot2 = new int[256];
        for (int i = 1; i < knot1.length; i++) {
            knot1[i] = i;
            knot2[i] = i;
        }
    }
}
