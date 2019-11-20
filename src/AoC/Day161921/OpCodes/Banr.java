package AoC.Day161921.OpCodes;

import AoC.Day161921.Memory;

public class Banr extends OpcodeFunction {

    @Override
    public Memory applySpecific() {
        int a = registers[instruction.getA()];
        int b = registers[instruction.getB()];
        int result = a & b;
        registers[instruction.getOutput()] = result;
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Banr";
    }
}
