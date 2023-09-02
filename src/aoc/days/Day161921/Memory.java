package aoc.days.Day161921;

import java.util.Arrays;

public class Memory {
    private int[] registers = new int[6];

    public Memory(int a, int b, int c, int d) {
        registers[0] = a;
        registers[1] = b;
        registers[2] = c;
        registers[3] = d;
    }

    public Memory(int a, int b, int c, int d, int e, int f) {
        registers[0] = a;
        registers[1] = b;
        registers[2] = c;
        registers[3] = d;
        registers[4] = e;
        registers[5] = f;
    }

    public int[] getRegisters() {
        return registers;
    }

    public Memory(int[] registers) {
        this.registers = registers;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Memory memory = (Memory) o;
        return Arrays.equals(registers, memory.registers);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(registers);
    }

    @Override
    public String toString() {
        return "Memory{" +
                "registers=" + Arrays.toString(registers) +
                '}';
    }
}
