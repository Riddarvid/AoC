package aoc21.days.day24.instructions;

import java.util.List;
import java.util.Map;
import java.util.Queue;

public class Input extends Instruction {
    public Input(List<String> parameters) {
        super(parameters);
    }

    @Override
    public void execute(Map<Character, Long> registers, Queue<Integer> inputQueue) {
        char targetRegister = parameters.get(0).charAt(0);
        registers.put(targetRegister, Long.valueOf(inputQueue.poll()));
    }
}
