package aoc21.days.day24;

import aoc21.days.day24.instructions.Instruction;

import java.util.*;

public class ALU {
    private final List<Instruction> instructions;
    private final List<Integer> inputSectionOffsets;

    public ALU(List<String> program) {
        List<Instruction> instructions = new ArrayList<>();
        List<Integer> inputSectionOffsets = new ArrayList<>();
        for (int rowNumber = 0; rowNumber < program.size(); rowNumber++) {
            String instructionString = program.get(rowNumber);
            instructions.add(Instruction.parse(instructionString));
            if (instructionString.startsWith("inp")) {
                inputSectionOffsets.add(rowNumber);
            }
        }
        this.instructions = instructions;
        this.inputSectionOffsets = inputSectionOffsets;
    }

    public Map<Character, Long> execute(String input) {
        Map<Character, Long> registers = new HashMap<>();
        registers.put('w', 0L);
        registers.put('x', 0L);
        registers.put('y', 0L);
        registers.put('z', 0L);
        Queue<Integer> inputQueue = new ArrayDeque<>();
        for (char c : input.toCharArray()) {
            inputQueue.add(Integer.parseInt("" + c));
        }
        for (Instruction instruction : instructions) {
            instruction.execute(registers, inputQueue);
        }
        return registers;
    }

    public Set<State> generateStates(State inputState) {
        Set<State> newStates = new HashSet<>();
        for (int i = 9; i >= 1; i--) {
            newStates.add(executeInputSection(inputState, i, inputState.z()));
        }
        return newStates;
    }

    private State executeInputSection(State inputState, int input, long z) {
        int inputSection = inputState.inputString().length();
        int startRow = inputSectionOffsets.get(inputSection);
        int endRow;
        if (inputSection == 13) {
            endRow = instructions.size();
        } else {
            endRow = inputSectionOffsets.get(inputSection + 1);
        }

        Map<Character, Long> registers = new HashMap<>();
        registers.put('w', 0L);
        registers.put('x', 0L);
        registers.put('y', 0L);
        registers.put('z', z);

        Queue<Integer> inputQueue = new ArrayDeque<>();
        inputQueue.add(input);

        for (int i = startRow; i < endRow; i++) {
            Instruction instruction = instructions.get(i);
            instruction.execute(registers, inputQueue);
        }
        return new State(inputState.inputString() + input, registers.get('z'));
    }
}
