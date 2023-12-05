package days.day14;

import days.day14.instructions.*;
import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day14 extends Day {
    List<Instruction> instructions1;
    List<Instruction> instructions2;

    public static void main(String[] args) {
        new Day14().runAndPrint();
    }

    @Override
    public long part1() {
        Computer computer = new Computer();
        for (Instruction instruction : instructions1) {
            instruction.apply(computer);
        }
        return computer.getSum();
    }

    @Override
    public long part2() {
        Computer computer = new Computer();
        for (Instruction instruction : instructions2) {
            instruction.apply(computer);
        }
        return computer.getSum();
    }

    @Override
    public void setup() {
        instructions1 = new ArrayList<>();
        instructions2 = new ArrayList<>();
        for (String s : lines) {
            if (s.startsWith("me")) {
                List<Long> longs = ParsingUtils.getLongs(s);
                instructions1.add(new MemoryInstruction(longs.get(0), longs.get(1)));
                instructions2.add(new MemoryInstruction(longs.get(0), longs.get(1)));
            } else {
                List<String> tokens = ParsingUtils.getTokens(s, ' ');
                String mask = tokens.get(2);
                instructions1.add(new MaskInstruction(new MaskV1(mask)));
                instructions2.add(new MaskInstruction(new MaskV2(mask)));
            }
        }
    }
}
