package aoc.days.day12.vm;

import java.util.Map;

public class JnzRegister implements Instruction {
    private final char conditionRegister;
    private final int offset;

    public JnzRegister(char conditionRegister, int offset) {
        this.conditionRegister = conditionRegister;
        this.offset = offset;
    }

    @Override
    public void execute(Map<Character, Integer> registers, PC pc) {
        if (registers.get(conditionRegister) != 0) {
            int oldPosition = pc.getPosition();
            pc.setPosition(oldPosition + offset);
        } else {
            pc.incPosition();
        }
    }
}
