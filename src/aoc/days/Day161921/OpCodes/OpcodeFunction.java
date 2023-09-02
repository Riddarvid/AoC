package aoc.days.Day161921.OpCodes;

import aoc.days.Day161921.Instruction;
import aoc.days.Day161921.Memory;
import aoc.days.Day161921.Setup;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;

public abstract class OpcodeFunction implements Function<Setup, Memory> {
    protected int[] registers;
    protected Instruction instruction;

    @Override
    public Memory apply(Setup setup) {
        registers = setup.getMemory().getRegisters();
        instruction = setup.getInstruction();
        return applySpecific();
    }

    abstract Memory applySpecific();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OpcodeFunction that = (OpcodeFunction) o;
        return Arrays.equals(registers, that.registers) &&
                Objects.equals(instruction, that.instruction);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(instruction);
        result = 31 * result + Arrays.hashCode(registers);
        return result;
    }

    @Override
    public String toString() {
        return instruction.toString();
    }
}
