package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public class Set extends Instruction {
    private char destination;
    private String value;

    public Set(char destination, String value) {
        this.destination = destination;
        this.value = value;
    }

    @Override
    public void execute(VM vm) {
        long source = valueOf(value, vm);
        vm.setRegister(destination, source);
        vm.incInstructionPointer();
    }
}
