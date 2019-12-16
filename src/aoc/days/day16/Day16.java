package aoc.days.day16;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day16 extends Day {
    private int[] basePattern;
    private int[] inputSignal;

    public static void main(String[] args) {
        new Day16();
    }

    @Override
    protected void part1() {
        int[] input = Arrays.copyOf(inputSignal, inputSignal.length);
        for (int i = 0; i < 100; i++) {
            input = phase(input);
        }
        printArr(input, 8);
    }

    private int[] phase(int[] input) {
        int[] output = new int[input.length];
        List<int[]> patterns = generatePatterns(basePattern, input.length);
        for (int i = 0; i < input.length; i++) {
            output[i] = generateNext(input, patterns.get(i));
        }
        return output;
    }

    private int generateNext(int[] input, int[] pattern) {
        int sum = 0;
        for (int i = 0; i < input.length; i++) {
            sum += input[i] * (pattern[(i + 1) % pattern.length]);
        }
        String s = Integer.toString(sum);
        s = s.substring(s.length() - 1);
        return Integer.parseInt(s);
    }

    private List<int[]> generatePatterns(int[] basePattern, int nPatterns) {
        List<int[]> patterns = new ArrayList<>();
        for (int i = 0; i < nPatterns; i++) {
            patterns.add(repeat(basePattern, i + 1));
        }
        return patterns;
    }

    private int[] repeat(int[] basePattern, int n) {
        int[] pattern = new int[basePattern.length * n];
        for (int i = 0; i < basePattern.length; i++) {
            for (int j = i * n; j < n + i * n; j++) {
                pattern[j] = basePattern[i];
            }
        }
        return pattern;
    }

    private void printArr(int[] arr, int n) {
        for (int i = 0; i < n; i++) {
            System.out.print(arr[i]);
        }
        System.out.println();
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        basePattern = new int[4];
        basePattern[0] = 0;
        basePattern[1] = 1;
        basePattern[2] = 0;
        basePattern[3] = -1;
        String s = lines.get(0);
        inputSignal = new int[s.length()];
        for (int i = 0; i < s.length(); i++) {
            inputSignal[i] = s.charAt(i) - '0';
        }
    }
}
