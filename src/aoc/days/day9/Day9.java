package aoc.days.day9;

import riddarvid.aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

public class Day9 extends Day implements Controller {
    private long[] originalProgram;
    private boolean part1;

    public static void main(String[] args) {
        new Day9();
    }

    @Override
    protected void part1() {
        part1 = true;
        new IntcodeComputer(this, originalProgram).execute();
    }

    @Override
    protected void part2() {
        part1 = false;
        new IntcodeComputer(this, originalProgram).execute();
    }

    @Override
    protected void setup() {
        originalProgram = InputUtils.readProgram(lines);
    }

    @Override
    public long getInput() {
        if (part1) {
            return 1;
        } else {
            return 2;
        }
    }

    @Override
    public void output(long val) {
        System.out.println(val);
    }
}
