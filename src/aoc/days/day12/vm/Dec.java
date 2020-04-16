package aoc.days.day12.vm;

import java.util.Map;

public class Dec implements Instruction {
    private final char targetRegister;

    public Dec(char targetRegister) {
        this.targetRegister = targetRegister;
    }

    @Override
    public void execute(Map<Character, Integer> registers, PC pc) {
        int value = registers.get(targetRegister);
        value--;
        registers.put(targetRegister, value);
        pc.incPosition();
    }
}
