package aoc.days.day5;

import aoc.days.Day;
import aoc.utils.intcode.Controller;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Day5 extends Day implements Controller {
    private long[] originalProgram;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        long[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        new IntcodeComputer(this, program, false).execute();
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        originalProgram = InputUtils.readProgram(lines);
    }

    @Override
    public long getInput() {
        System.out.println("Please enter an int\n");
        Scanner sc = new Scanner(System.in);
        return Integer.parseInt(sc.nextLine());
    }

    @Override
    public void output(long val) {
        System.out.println(val);
    }
}
