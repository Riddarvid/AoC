package aoc.days.day22;

import riddarvid.aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.math.Tuple;

import java.util.*;

public class Day22 extends Day {
    private List<Instruction> instructions;

    public static void main(String[] args) {
        new Day22();
    }

    @Override
    protected void part1() {
        long startIndex = 2019;
        long size = 10007;
        Tuple<Long, Long> f = getFunction(instructions, size);
        System.out.println(applyFunction(f, size, startIndex));
        System.out.println();
    }

    @Override
    protected void part2() {
        long size = 119315717514047L;
        Tuple<Long, Long> f = getFunction(instructions, size);
        long a = f.getFirst();
        long b = f.getSecond();
        System.out.println("One shuffle: " + a + " * x + " + b);
        //a and b become startA and startB respectively in Affine.hs
    }

    private long applyFunction(Tuple<Long, Long> f, long size, long startIndex) {
        return (f.getFirst() * startIndex + f.getSecond()) % size;
    }

    private Tuple<Long, Long> getFunction(List<Instruction> instructions, long size) {
        long a = 1;
        long b = 0;
        for (Instruction instruction : instructions) {
            long c = instruction.getValue();
            switch (instruction.getType()) {
                case REVERSE:
                    a = size - a;
                    b = size - 1 - b;
                    break;
                case CUT:
                    b = size + (b - c);
                    break;
                case INC:
                    a = a * c;
                    b = b * c;
                    break;
            }
            a = a % size;
            b = b % size;
        }
        return new Tuple<>(a, b);
    }

    @Override
    protected void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            List<Long> values = InputUtils.getIntsNegative(s);
            if (s.charAt(0) == 'c') {
                long value = values.get(0);
                instructions.add(new Instruction(Type.CUT, value));
            } else if (s.charAt(5) == 'i') {
                instructions.add(new Instruction(Type.REVERSE));
            } else {
                long value = values.get(0);
                instructions.add(new Instruction(Type.INC, value));
            }
        }
    }
}
