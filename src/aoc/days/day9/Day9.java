package aoc.days.day9;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Scanner;

public class Day9 extends Day implements Controller {
    private long[] originalProgram;

    public static void main(String[] args) {
        new Day9();
    }

    @Override
    protected void part1() {
        IntcodeComputer ic = new IntcodeComputer(this, originalProgram, false);
        ic.execute();
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
