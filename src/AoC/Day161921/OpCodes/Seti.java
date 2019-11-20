package AoC.Day161921.OpCodes;

import AoC.Day161921.Memory;

public class Seti extends OpcodeFunction {
    @Override
    public Memory applySpecific() {
        registers[instruction.getOutput()] = instruction.getA();
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Seti";
    }
}
