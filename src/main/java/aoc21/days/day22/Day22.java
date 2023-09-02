package aoc21.days.day22;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day22 extends Day {
    private List<Instruction> initInstructions;
    private List<Instruction> instructions;

    @Override
    public void setup() {
        initInstructions = new ArrayList<>();
        instructions = new ArrayList<>();
        boolean initializing = true;
        for (String s : lines) {
            Instruction.State state;
            if (s.charAt(1) == 'n') {
                state = Instruction.State.ON;
            } else {
                state = Instruction.State.OFF;
            }
            List<Integer> values = ParsingUtils.getIntegersNegative(s);
            if (initializing) {
                for (int value : values) {
                    if (value < -50 || value > 50) {
                        initializing = false;
                        break;
                    }
                }
            }
            Cuboid cuboid = new Cuboid(values.get(0), values.get(1), values.get(2), values.get(3), values.get(4), values.get(5));
            Instruction instruction = new Instruction(state, cuboid);
            if (initializing) {
                initInstructions.add(instruction);
            } else {
                instructions.add(instruction);
            }
        }
    }

    @Override
    public long part1() {
        Reactor reactor = new Reactor();
        for (Instruction instruction : initInstructions) {
            reactor.executeInstruction(instruction);
        }
        return reactor.getNumberOfLit();
    }

    @Override
    public long part2() {
        Reactor reactor = new Reactor();
        for (Instruction instruction : initInstructions) {
            reactor.executeInstruction(instruction);
        }
        for (Instruction instruction : instructions) {
            reactor.executeInstruction(instruction);
        }
        return reactor.getNumberOfLit();
    }
}
