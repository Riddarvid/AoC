package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public class Add extends Instruction {
    private char destination;
    private String source;

    public Add(char destination, String source) {
        this.destination = destination;
        this.source = source;
    }

    @Override
    public void execute(VM vm) {
        long value = valueOf(source, vm);
        long earlier = vm.getRegister(destination);
        vm.setRegister(destination, earlier + value);
        vm.incInstructionPointer();
    }
}
