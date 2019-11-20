package aoc.days.day18.instructions;

import aoc.days.day18.VM;

public class Rcv extends Instruction {
    private char register;

    public Rcv(char register) {
        this.register = register;
    }

    @Override
    public void execute(VM vm) {
        synchronized (vm) {
            if (vm.hasMessage()) {
                vm.setRegister(register, vm.getLastMessage());
                vm.incInstructionPointer();
            }
        }
    }
}
