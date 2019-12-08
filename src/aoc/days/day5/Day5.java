package aoc.days.day5;

import aoc.days.Day;
import aoc.utils.intcode.Controller;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Day5 extends Day implements Controller {
    private int[] originalProgram;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        int[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        new IntcodeComputer(this).execute(program);
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtils.getIntsNegative(s);
        originalProgram = new int[values.size()];
        for (int i = 0; i < originalProgram.length; i++) {
            originalProgram[i] = values.get(i);
        }
    }

    @Override
    public int getInput() {
        System.out.println("Please enter an int\n");
        Scanner sc = new Scanner(System.in);
        return Integer.parseInt(sc.nextLine());
    }

    @Override
    public void output(int val) {
        System.out.println(val);
    }
}
