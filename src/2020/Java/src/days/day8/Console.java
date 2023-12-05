package days.day8;

import days.day8.instructions.Instruction;

import java.util.List;

public class Console {
    private long accumulator = 0;
    private int pc = 0;
    private final List<Instruction> instructions;

    public Console(List<Instruction> instructions) {
        this.instructions = instructions;
    }

    public void step() {
        instructions.get(pc).apply(this);
    }

    public long getAccumulator() {
        return accumulator;
    }

    public void setAccumulator(long accumulator) {
        this.accumulator = accumulator;
    }

    public int getPc() {
        return pc;
    }

    public void setPc(int pc) {
        this.pc = pc;
    }

    public void incPc() {
        pc++;
    }

    public void run() {
        while (pc < instructions.size()) {
            instructions.get(pc).apply(this);
        }
        if (pc != instructions.size()) {
            throw new IllegalStateException("Program terminated incorrectly");
        }
    }
}
