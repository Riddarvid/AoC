package aoc.days.day2;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.Arrays;
import java.util.List;

import static aoc.utils.IntcodeComputer.execute;

public class Day2 extends Day {
    private int[] originalProgram;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        int[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        program[1] = 12;
        program[2] = 2;
        execute(program);
        System.out.println(program[0]);
    }

    @Override
    protected void part2() {
        int noun = 0;
        int verb = 0;
        int[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        for (noun = 0; noun < 99; noun++) {
            for (verb = 0; verb < 99; verb++) {
                program = Arrays.copyOf(originalProgram, originalProgram.length);
                program[1] = noun;
                program[2] = verb;
                execute(program);
                if (program[0] == 19690720) {
                    break;
                }
            }
            if (program[0] == 19690720) {
                    break;
            }
        }
        System.out.println(100 * noun + verb);
    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtilities.getInts(s);
        originalProgram = new int[values.size()];
        for (int i = 0; i < originalProgram.length; i++) {
            originalProgram[i] = values.get(i);
        }
    }
}
