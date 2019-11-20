package AoC.Day161921.OpCodes;

import AoC.Day161921.Memory;

public class Mulr extends OpcodeFunction {

    @Override
    public Memory applySpecific() {
        int result = registers[instruction.getA()];
        result *= registers[instruction.getB()];
        registers[instruction.getOutput()] = result;
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Mulr";
    }
}
