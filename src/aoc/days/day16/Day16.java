package aoc.days.day16;

import riddarvid.aoc.days.Day;

import java.util.*;

public class Day16 extends Day {
    private int[] basePattern;
    private int[] inputSignal;

    public static void main(String[] args) {
        new Day16();
    }

    @Override
    protected void part1() {/*
        int[] input = Arrays.copyOf(inputSignal, inputSignal.length);
        System.out.println(Arrays.toString(input));
        for (int i = 0; i < 100; i++) {
            input = phase(input);
            System.out.println(Arrays.toString(input));
        }
        printArr(input, 8);*/
    }

    /*
    private int[] phase(int[] input) {
        int[] output = new int[input.length];
        for (int i = 0; i < input.length; i++) {
            output[i] = generateNext(input, i + 1);
        }
        return output;
    }
*/
    private int generateNext(List<Integer> input, int patternLength) {
        int sum = 0;
        for (int i = 0; i < input.size(); i++) {
            switch (getModifier(i, patternLength)) {
                case 1:
                    sum += input.get(i);
                    break;
                case -1:
                    sum -= input.get(i);
                    break;
            }
        }
        return Math.abs(sum) % 10;
    }

    private int getModifier(int i, int pattern) {
        i++;
        int index = i % (pattern * 4);
        if (index >= 0 && index < pattern) {
            return 0;
        } else if (index >= pattern && index < 2 * pattern) {
            return 1;
        } else if (index >= 2 * pattern && index < 3 * pattern) {
            return 0;
        } else {
            return -1;
        }
    }

    private int generateNext(List<Integer> input, int[] pattern) {
        int sum = 0;
        for (int i = 0; i < input.size(); i++) {
            sum += input.get(i) * (pattern[(i + 1) % pattern.length]);
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
        int skipSize = getStartIndex(inputSignal);
        int[] reduced = reduce(inputSignal, skipSize);
        reduced = reverse(reduced);
        for (int i = 0; i < 100; i++) {
            reduced = phase2(reduced);
        }
        reduced = reverse(reduced);
        printArr(reduced, 8);
    }

    private int[] phase2(int[] reduced) {
        int[] output = new int[reduced.length];
        int sumSoFar = 0;
        for (int i = 0; i < reduced.length; i++) {
            sumSoFar = Math.abs(sumSoFar + reduced[i]) % 10;
            output[i] = sumSoFar;
        }
        return output;
    }

    private int getNext(int index, int[] reduced) {
        int sum = 0;
        for (int i = 0; i <= index; i++) { //Left shift -> <=
            sum += reduced[i];
        }
        return Math.abs(sum) % 10;
    }

    private int[] reverse(int[] reduced) {
        int[] reversed = new int[reduced.length];
        for (int i = 0; i < reduced.length; i++) {
            reversed[i] = reduced[reduced.length - 1 - i];
        }
        return reversed;
    }

    private int[] reduce(int[] inputSignal, int skipSize) {
        inputSignal = repeatArr(inputSignal, 10000);
        int[] reduced = new int[inputSignal.length - skipSize];
        System.arraycopy(inputSignal, skipSize, reduced, 0, inputSignal.length - skipSize);
        return reduced;
    }

    private int getStartIndex(int[] inputSignal) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 7; i++) {
            sb.append(inputSignal[i]);
        }
        return Integer.parseInt(sb.toString());
    }

    private int sum(int[] inputSignal) {
        int sum = 0;
        for (int n : inputSignal) {
            sum += n;
        }
        return sum;
    }

    private void printList(List<Integer> input, int size) {
        for (int i = 0; i < size; i++) {
            System.out.print(input.get(i));
        }
        System.out.println();
    }

    private int findPeriod(List<Integer> input) {
        Set<List<Integer>> earlier = new HashSet<>();
        earlier.add(input);
        int cycle = 0;
        while (true) {
            cycle++;
            input = phase(input);
            if (earlier.contains(input)) {
                break;
            }
            earlier.add(input);
        }
        return cycle;
    }

    private List<Integer> phase(List<Integer> input) {
        List<Integer> output = new ArrayList<>(input.size());
        for (int i = 0; i < input.size(); i++) {
            output.add(generateNext(input, i + 1));
        }
        return output;
    }

    private int[] repeatArr(int[] inputSignal, int n) {
        int[] output = new int[inputSignal.length * n];
        for (int repetition = 0; repetition < n; repetition++) {
            System.arraycopy(inputSignal, 0, output, repetition * inputSignal.length, inputSignal.length);
        }
        return output;
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
