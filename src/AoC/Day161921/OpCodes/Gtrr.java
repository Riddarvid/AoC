package AoC.Day161921.OpCodes;

import AoC.Day161921.Memory;

public class Gtrr extends OpcodeFunction {
    @Override
    public Memory applySpecific() {
        if (registers[instruction.getA()] > registers[instruction.getB()]) {
            registers[instruction.getOutput()] = 1;
        } else {
            registers[instruction.getOutput()] = 0;
        }
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Gtrr";
    }
}
