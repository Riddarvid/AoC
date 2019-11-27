package aoc.days.day18;

import aoc.days.day18.instructions.Instruction;

import java.util.List;

public class Executer implements Runnable {
    private VM vm;
    private List<Instruction> instructions;

    public Executer(VM vm, List<Instruction> instructions) {
        this.vm = vm;
        this.instructions = instructions;
    }

    public void run() {
        while (vm.isRunning()) {
            int instructionPointer = vm.getInstructionPointer();
            instructions.get(instructionPointer).execute(vm);
        }
    }
}
