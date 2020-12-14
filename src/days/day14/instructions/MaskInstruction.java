package days.day14.instructions;

import days.day14.Computer;

public class MaskInstruction implements Instruction {
    private Mask mask;

    public MaskInstruction(Mask mask) {
        this.mask = mask;
    }

    @Override
    public void apply(Computer computer) {
        computer.setMask(mask);
    }
}
