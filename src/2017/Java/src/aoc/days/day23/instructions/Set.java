package aoc.days.day23.instructions;

import aoc.days.day23.VM;

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
        vm.put(destination, source);
        vm.incInstructionPointer();
    }

    @Override
    public String toString() {
        return "Set " + destination + " to " + value;
    }
}
