package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public class Rcv extends Instruction {
    private char register;

    public Rcv(char register) {
        this.register = register;
    }

    @Override
    public void execute(VM vm) {
        vm.receive(register);
        vm.incInstructionPointer();
    }

    @Override
    public String toString() {
        return "Recieve if " + register + " != 0";
    }
}
