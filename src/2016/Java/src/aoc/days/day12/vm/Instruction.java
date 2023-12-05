package aoc.days.day12.vm;

import java.util.Map;

public interface Instruction {
    void execute(Map<Character, Integer> registers, PC pc);
}
