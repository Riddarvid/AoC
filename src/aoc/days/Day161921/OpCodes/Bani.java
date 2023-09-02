package aoc.days.Day161921.OpCodes;

import aoc.days.Day161921.Memory;

public class Bani extends OpcodeFunction {

    @Override
    public Memory applySpecific() {
        int a = registers[instruction.getA()];
        int b = instruction.getB();
        int result = a & b;
        registers[instruction.getOutput()] = result;
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Bani";
    }
}
