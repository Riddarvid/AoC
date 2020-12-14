package days.day14.instructions;

import days.day14.Computer;

public class MemoryInstruction implements Instruction {
    private final long address;
    private final long value;

    public MemoryInstruction(long address, long value) {
        this.address = address;
        this.value = value;
    }

    @Override
    public void apply(Computer computer) {
        computer.updateMemory(address, value);
    }
}
