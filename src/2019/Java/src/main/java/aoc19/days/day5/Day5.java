package aoc19.days.day5;

import aoc19.utils.input.InputUtils;
import aoc19.utils.intcode.Controller;
import aoc19.utils.intcode.IntcodeComputer;
import riddarvid.aoc.days.Day;

import java.util.Arrays;

public class Day5 extends Day implements Controller {
    private long[] originalProgram;
    private boolean part1;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        long[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        new IntcodeComputer(this, program).execute();
        System.out.println();
    }

    @Override
    protected void part2() {
        long[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        part1 = false;
        new IntcodeComputer(this, program).execute();
    }

    @Override
    protected void setup() {
        originalProgram = InputUtils.readProgram(lines);
        part1 = true;
    }

    @Override
    public long getInput() {
        if (part1) {
            return 1;
        } else {
            return 5;
        }
    }

    @Override
    public void output(long val) {
        System.out.println(val);
    }
}
