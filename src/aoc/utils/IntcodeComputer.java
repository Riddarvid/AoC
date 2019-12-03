package aoc.utils;

public class IntcodeComputer {
    public static void execute(int[] program) {
        int i = 0;
        while (program[i] != 99) {
            if (program[i] == 1) {
                add(program[i + 1], program[i + 2], program[i + 3], program);
            } else if (program[i] == 2) {
                mul(program[i + 1], program[i + 2], program[i + 3], program);
            }
            i += 4;
        }
    }

    private static void add(int op1, int op2, int dest, int[] program) {
        program[dest] = program[op1] + program[op2];
    }

    private static void mul(int op1, int op2, int dest, int[] program) {
        program[dest] = program[op1] * program[op2];
    }
}
