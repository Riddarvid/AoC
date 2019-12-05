package aoc.days.day5;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.Arrays;
import java.util.List;

import static aoc.utils.IntcodeComputer.execute;

public class Day5 extends Day {
    private int[] originalProgram;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        int[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        execute(program);
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtilities.getIntsNegative(s);
        originalProgram = new int[values.size()];
        for (int i = 0; i < originalProgram.length; i++) {
            originalProgram[i] = values.get(i);
        }
    }
}
