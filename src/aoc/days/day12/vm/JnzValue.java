package aoc.days.day12.vm;

import java.util.Map;

public class JnzValue implements Instruction {
    private final int conditionValue;
    private final int offset;

    public JnzValue(int conditionValue, int offset) {
        this.conditionValue = conditionValue;
        this.offset = offset;
    }

    @Override
    public void execute(Map<Character, Integer> registers, PC pc) {
        if (conditionValue != 0) {
            int oldPosition = pc.getPosition();
            pc.setPosition(oldPosition + offset);
        } else {
            pc.incPosition();
        }
    }
}
