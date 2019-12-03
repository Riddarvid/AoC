package aoc.days.day23;

public class VM {
    private long[] registers;
    private int instructionPointer;
    private int mulCount;

    public VM() {
        registers = new long[8];
        instructionPointer = 0;
        mulCount = 0;
    }

    public int getInstructionPointer() {
        return instructionPointer;
    }

    public void setInstructionPointer(int instructionPointer) {
        this.instructionPointer = instructionPointer;
    }

    public void incInstructionPointer() {
        instructionPointer++;
    }

    public long get(char register) {
        return registers[register - 'a'];
    }

    public void put(char register, long value) {
        registers[register - 'a'] = value;
    }

    public void incMulCount() {
        mulCount++;
    }

    public int mulCount() {
        return mulCount;
    }
}
