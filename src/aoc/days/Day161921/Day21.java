package aoc.days.Day161921;

import aoc.days.Day161921.OpCodes.*;
import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day21 {
    public static void main(String[] args) {
        new Day21();
    }

    public Day21() {
        Map<String, OpcodeFunction> operations = new HashMap<>();
        operations.put("addi", new Addi());
        operations.put("addr", new Addr());
        operations.put("bani", new Bani());
        operations.put("banr", new Banr());
        operations.put("bori", new Bori());
        operations.put("borr", new Borr());
        operations.put("eqir", new Eqir());
        operations.put("eqri", new Eqri());
        operations.put("eqrr", new Eqrr());
        operations.put("gtir", new Gtir());
        operations.put("gtri", new Gtri());
        operations.put("gtrr", new Gtrr());
        operations.put("muli", new Muli());
        operations.put("mulr", new Mulr());
        operations.put("seti", new Seti());
        operations.put("setr", new Setr());

        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day161921/Input21.txt"));
        int instructionPointerRegister = RegEx.getIntegers(input[0]).get(0);
        int instructionPointer = 0;
        KnownInstruction[] program = new KnownInstruction[input.length - 1];
        for (int i = 1; i < input.length; i++) {
            String s = input[i];
            OpcodeFunction operation = operations.get(s.substring(0, 4));
            List<Integer> values = RegEx.getIntegers(s);
            program[i - 1] = new KnownInstruction(operation, values.get(0), values.get(1), values.get(2));
        }
        Memory memory = new Memory(0, 0, 0, 0, 0, 0);
        List<Integer> discoveredValuesOfD = new ArrayList<>();
        while (instructionPointer < program.length && instructionPointer >= 0) {
            if (instructionPointer == 28) {
                System.out.print(memory.getRegisters()[3]);
                if (discoveredValuesOfD.contains(memory.getRegisters()[3])) {
                    System.out.println(" Has been discovered");
                } else {
                    System.out.println(" Has not been discovered");
                    discoveredValuesOfD.add(memory.getRegisters()[3]);
                }
            }
            memory.getRegisters()[instructionPointerRegister] = instructionPointer;
            memory = program[instructionPointer].apply(memory);
            instructionPointer = memory.getRegisters()[instructionPointerRegister];
            instructionPointer++;
        }
    }
}
