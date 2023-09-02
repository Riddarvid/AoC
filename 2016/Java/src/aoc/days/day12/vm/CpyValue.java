package aoc.days.day12.vm;

import java.util.Map;

public class CpyValue implements Instruction {
    private final int value;
    private final char targetRegister;

    public CpyValue(int value, char targetRegister) {
        this.value = value;
        this.targetRegister = targetRegister;
    }

    @Override
    public void execute(Map<Character, Integer> registers, PC pc) {
        registers.put(targetRegister, value);
        pc.incPosition();
    }
}
