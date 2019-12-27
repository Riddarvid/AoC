package aoc.days.day02;

import aoc.days.Day;
import aoc.utils.intcode.Controller;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.List;

public class Day2 extends Day implements Controller {
    private long[] originalProgram;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        long[] program = Arrays.copyOf(originalProgram, originalProgram.length);
        program[1] = 12;
        program[2] = 2;
        IntcodeComputer ic = new IntcodeComputer(this, program);
        ic.execute();
        System.out.println(ic.getMemory(0));
    }

    @Override
    protected void part2() {
        int noun = 0;
        int verb = 0;
        IntcodeComputer ic = new IntcodeComputer(this, originalProgram);
        for (noun = 0; noun < 99; noun++) {
            for (verb = 0; verb < 99; verb++) {
                long[] program = Arrays.copyOf(originalProgram, originalProgram.length);
                program[1] = noun;
                program[2] = verb;
                ic = new IntcodeComputer(this, program);
                ic.execute();
                if (ic.getMemory(0) == 19690720) {
                    break;
                }
            }
            if (ic.getMemory(0) == 19690720) {
                    break;
            }
        }
        System.out.println(100 * noun + verb);
    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtils.getInts(s);
        originalProgram = new long[values.size()];
        for (int i = 0; i < originalProgram.length; i++) {
            originalProgram[i] = values.get(i);
        }
    }

    @Override
    public long getInput() {
        return 0; //Is not used
    }

    @Override
    public void output(long val) { //Is not used
    }
}
