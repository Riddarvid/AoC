package aoc.days.day23;

import aoc.days.day23.instructions.Instruction;

import java.util.List;

public class Executer {
    private VM vm;
    private List<Instruction> instructions;

    public Executer(VM vm, List<Instruction> instructions) {
        this.vm = vm;
        this.instructions = instructions;
    }

    public void run() {
        int instructionPointer = vm.getInstructionPointer();
        while (instructionPointer >= 0 && instructionPointer < instructions.size()) {
            instructions.get(instructionPointer).execute(vm);
            instructionPointer = vm.getInstructionPointer();
        }
        System.out.println(vm.mulCount());
    }
}
