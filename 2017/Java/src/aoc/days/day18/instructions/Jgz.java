package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public class Jgz extends Instruction {
    private String destination;
    private String condition;

    public Jgz(String condition, String destination) {
        this.destination = destination;
        this.condition = condition;
    }

    @Override
    public void execute(VM vm) {
        if (valueOf(condition, vm) > 0) {
            int earlierIP = vm.getInstructionPointer();
            vm.setInstructionPointer(earlierIP + (int)valueOf(destination, vm));
        } else {
            vm.incInstructionPointer();
        }
    }

    @Override
    public String toString() {
        return "Jump " + destination + " if " + condition + " > 0";
    }
}
