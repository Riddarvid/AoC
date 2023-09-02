package days.day8.instructions;

import days.day8.Console;

public class Jmp implements Instruction {
    private final int offset;

    public Jmp(int offset) {
        this.offset = offset;
    }


    @Override
    public void apply(Console console) {
        console.setPc(console.getPc() + offset);
    }

    @Override
    public String getName() {
        return "jmp";
    }

    @Override
    public int getValue() {
        return offset;
    }
}
