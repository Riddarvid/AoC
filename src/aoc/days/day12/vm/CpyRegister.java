package aoc.days.day12.vm;

import java.util.Map;

public class CpyRegister implements Instruction {
    private final char sourceRegister;
    private final char targetRegister;

    public CpyRegister(char sourceRegister, char targetRegister) {
        this.sourceRegister = sourceRegister;
        this.targetRegister = targetRegister;
    }

    @Override
    public void execute(Map<Character, Integer> registers, PC pc) {
        int value = registers.get(sourceRegister);
        registers.put(targetRegister, value);
        pc.incPosition();
    }
}
