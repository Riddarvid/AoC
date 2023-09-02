package aoc.days.day6;

import aoc.utils.InputUtilities;
import aoc.days.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;

public class Day6 extends Day {
    int[] banks;
    List<int[]> configurations;

    public static void main(String[] args) {
        new Day6();
    }

    @Override
    protected void part1() {
        configurations = new ArrayList<>();
        configurations.add(Arrays.copyOf(banks, banks.length));
        int redistributions = 0;
        while (true){
            redistributions++;
            redistribute(banks);
            if (contains(configurations, banks)) {
                break;
            }
            configurations.add(Arrays.copyOf(banks, banks.length));
        }
        System.out.println(redistributions);
    }

    private boolean contains(List<int[]> configurations, int[] banks) {
        for (int[] configuration : configurations) {
            if (Arrays.equals(configuration, banks)) {
                return true;
            }
        }
        return false;
    }

    private void redistribute(int[] banks) {
        int toRedistribute = 0;
        for (int i = 1; i < banks.length; i++) {
            if (banks[i] > banks[toRedistribute]) {
                toRedistribute = i;
            }
        }
        int blocks = banks[toRedistribute];
        banks[toRedistribute] = 0;
        int i = toRedistribute;
        while (blocks > 0) {
            i = (i + 1) % banks.length;
            banks[i]++;
            blocks--;
        }
    }

    @Override
    protected void part2() {
        System.out.println(configurations.size() - indexOf(configurations, banks));
    }

    private int indexOf(List<int[]> configurations, int[] banks) {
        for (int i = 0; i < configurations.size(); i++) {
            if (Arrays.equals(configurations.get(i), banks)) {
                return i;
            }
        }
        throw new InputMismatchException();
    }

    @Override
    protected void setup() {
        List<String> initValues = InputUtilities.getTokens(lines.get(0), '\t');
        banks = new int[initValues.size()];
        for (int i = 0; i < initValues.size(); i++) {
            banks[i] = Integer.parseInt(initValues.get(i));
        }
    }
}
