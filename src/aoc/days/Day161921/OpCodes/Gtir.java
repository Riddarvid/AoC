package aoc.days.Day161921.OpCodes;

import aoc.days.Day161921.Memory;

public class Gtir extends OpcodeFunction {
    @Override
    public Memory applySpecific() {
        if (instruction.getA() > registers[instruction.getB()]) {
            registers[instruction.getOutput()] = 1;
        } else {
            registers[instruction.getOutput()] = 0;
        }
        return new Memory(registers);
    }

    @Override
    public String toString() {
        return "Gtir";
    }
}
