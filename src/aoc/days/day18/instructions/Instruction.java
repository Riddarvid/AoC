package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public abstract class Instruction {
    public abstract void execute(VM vm);

    long valueOf(String input, VM vm) {
        if (Character.isDigit(input.charAt(0)) || input.charAt(0) == '-') {
            return Long.parseLong(input);
        }
        return vm.getRegister(input.charAt(0));
    }
}
