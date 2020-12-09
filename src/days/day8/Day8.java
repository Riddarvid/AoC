package days.day8;

import days.day8.instructions.Acc;
import days.day8.instructions.Instruction;
import days.day8.instructions.Jmp;
import days.day8.instructions.Nop;
import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;

public class Day8 extends Day {
    private List<Instruction> instructions;

    public static void main(String[] args) {
        new Day8();
    }

    @Override
    public long part1() {
        Console console = new Console(instructions);
        Set<Integer> executed = new HashSet<>();
        while (!executed.contains(console.getPc())) {
            executed.add(console.getPc());
            console.step();
        }
        return console.getAccumulator();
    }

    @Override
    public long part2() {
        LinkedList<Instruction> instructionsCopy = new LinkedList<>(instructions);
        int index = 0;
        while (index < instructionsCopy.size()) {
            Instruction instruction = instructionsCopy.get(index);
            if (instruction.getName().equals("acc")) {
                index++;
                continue;
            }
            int value = instruction.getValue();
            Instruction opposite;
            if (instruction.getName().equals("nop")) {
                opposite = new Jmp(value);
            } else {
                opposite = new Nop(value);
            }
            instructionsCopy.remove(index);
            instructionsCopy.add(index, opposite);
            if (terminatesCorrectly(instructionsCopy)) {
                break;
            }
            instructionsCopy.remove(index);
            instructionsCopy.add(index, instruction);
            index++;
        }
        Console console = new Console(instructionsCopy);
        console.run();
        return console.getAccumulator();
    }

    private boolean terminatesCorrectly(LinkedList<Instruction> instructions) {
        Set<Integer> executed = new HashSet<>();
        Console console = new Console(instructions);
        while (console.getPc() < instructions.size()) {
            if (executed.contains(console.getPc())) {
                return false;
            }
            executed.add(console.getPc());
            console.step();
        }
        return console.getPc() == instructions.size();
    }

    @Override
    public void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            instructions.add(parseInstruction(s));
        }
    }

    private Instruction parseInstruction(String s) {
        String name = s.substring(0, 3);
        int value = ParsingUtils.getIntegersNegative(s).get(0);
        switch (name) {
            case "acc":
                return new Acc(value);
            case "jmp":
                return new Jmp(value);
            case "nop":
                return new Nop(value);
        }
        throw new InputMismatchException("Unsupported instruction: " + name);
    }
}
