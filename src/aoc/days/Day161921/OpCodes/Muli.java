package aoc.days.Day161921.OpCodes;

import aoc.days.Day161921.Memory;

public class Muli extends OpcodeFunction {

    @Override
    public Memory applySpecific() {
        int result = registers[instruction.getA()];
        result *= instruction.getB();
        registers[instruction.getOutput()] = result;
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Muli";
    }
}
