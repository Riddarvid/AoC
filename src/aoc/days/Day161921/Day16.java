package aoc.days.Day161921;

import aoc.days.Day161921.OpCodes.*;
import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.util.*;

public class Day16 {
    public static void main(String[] args) {
        new Day16();
    }

    public Day16() {
        OpcodeFunction[] functions = {
                new Addi(),
                new Addr(),
                new Bani(),
                new Banr(),
                new Bori(),
                new Borr(),
                new Eqir(),
                new Eqri(),
                new Eqrr(),
                new Gtir(),
                new Gtri(),
                new Gtrr(),
                new Muli(),
                new Mulr(),
                new Seti(),
                new Setr()
        };
        List<OpCode> discovered = new ArrayList<>();
        while (discovered.size() < 16) {
            OpCode[] opCodes = new OpCode[16];
            for (int i = 0; i < opCodes.length; i++) {
                opCodes[i] = new OpCode(i);
            }
            String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day161921/input.txt"));
            List<TestCase> cases = new ArrayList<>();
            for (int i = 0; i < input.length; i++) {
                List<Integer> before = RegEx.getIntegers(input[i]);
                i++;
                List<Integer> instruction = RegEx.getIntegers(input[i]);
                i++;
                List<Integer> after = RegEx.getIntegers(input[i]);
                i++;
                Memory memoryBefore = new Memory(before.get(0), before.get(1), before.get(2), before.get(3));
                Memory memoryAfter = new Memory(after.get(0), after.get(1), after.get(2), after.get(3));
                Instruction instructionReal = new Instruction(instruction.get(0), instruction.get(1), instruction.get(2), instruction.get(3));
                cases.add(new TestCase(memoryBefore, memoryAfter, instructionReal));
            }
            int totalCounter = 0;
            for (TestCase t : cases) {
                int counter = 0;
                for (OpcodeFunction f : functions) {
                    if (!discovered.contains(new OpCode(f))) {
                        Memory actual = f.apply(t.setup);
                        Memory expected = t.expected;
                        if (actual.equals(expected)) {
                            counter++;
                            opCodes[t.setup.getInstruction().getOpCode()].addBehaviour(f);
                        }
                    }
                }
                if (counter >= 3) {
                    totalCounter++;
                }
            }
            for (OpCode c : opCodes) {
                if (c.getBehaviours().size() == 1 && !discovered.contains(c)) {
                    discovered.add(c);
                }
            }
        }
        OpcodeFunction[] codes = new OpcodeFunction[16];
        for (OpCode c : discovered) {
            codes[c.getCode()] = c.getBehaviours().get(0);
        }
        String[] input2 = FileUtilities.getArrayFromFile(new File("src/AoC/Day161921/Input2.txt"));
        Queue<Instruction> instructions = new LinkedList<>();
        for (String s : input2) {
            List<Integer> ints = RegEx.getIntegers(s);
            instructions.add(new Instruction(ints.get(0), ints.get(1), ints.get(2), ints.get(3)));
        }
        Memory memory = new Memory(0, 0, 0, 0);
        while (!instructions.isEmpty()) {
            Instruction instruction = instructions.poll();
            Setup setup = new Setup(memory, instruction);
            memory = codes[instruction.getOpCode()].apply(setup);
        }
        System.out.println(memory.toString());
    }
}
