package aoc.days.day22;

import aoc.days.Day;
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
        /*long startIndex = 2019;
        long endIndex = 6794;
        long size = 10007;
        System.out.println(shuffle(startIndex, size));
        System.out.println(antiShuffle(endIndex, size));
        System.out.println();*/
    }

    @Override
    protected void part2() {
        //System.out.println(antiInc(80044130060782L, 38660355580801L, 119315717514047L));
        long size = 119315717514047L;
        long times = 101741582076661L;
        Tuple<Long, Long> f = getFunction(instructions, size);
        System.out.println(f.getFirst() + " * x + " + f.getSecond());
        long a = f.getFirst();
        long b = f.getSecond();
    }

    private Tuple<Long, Long> getRepeatFunction(long originalA, long originalB, long size, long times) {
        long totalA = 1;
        long totalB = 0;
        while (times > 1) {
            long n = (long)(Math.log(times)/Math.log(2));
            long a = originalA;
            long b = originalB;
            for (int i = 0; i < n; i++) {
                long newA = (a * a) % size;
                long newB = (b * (a + 1)) % size;
                a = newA;
                b = newB;
            }
            totalA = (totalA * a) % size;
            totalB = totalB * a + b;
            times = times - (long)Math.pow(2, n);
        }
        return new Tuple<>(totalA, totalB);
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

    private long antiShuffle(long index, long size) {
        for (int i = instructions.size() - 1; i >= 0; i--) {
            Instruction instruction = instructions.get(i);
            switch (instruction.getType()) {
                case CUT:
                    index = antiCut(index, instruction.getValue(), size);
                    break;
                case REVERSE:
                    index = reverse(index, size);
                    break;
                case INC:
                    index = antiInc(index, instruction.getValue(), size);
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
        return index;
    }

    private long shuffle(long index, long size) {
        for (Instruction instruction : instructions) {
            switch (instruction.getType()) {
                case CUT:
                    index = cut(index, instruction.getValue(), size);
                    break;
                case REVERSE:
                    index = reverse(index, size);
                    break;
                case INC:
                    index = inc(index, instruction.getValue(), size);
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
        return index;
    }

    private long inc(long index, long value, long size) {
        return (index * value) % size;
    }

    private long antiInc(long index, long value, long size) {
        long factor = index;
        while (factor % value != 0) {
            factor += size;
        }
        return factor / value;
    }

    private long cut(long index, long value, long size) {
        if (value < 0) {
            return cut(index, size + value, size);
        }
        if (index < value) {
            return size - (value - index);
        }
        return index - value;
    }

    private long antiCut(long index, long value, long size) {
        if (value < 0) {
            return antiCut(index, size + value, size);
        }
        long posFromEnd = size - 1 - index;
        if (posFromEnd < value) {
            return value - 1 - posFromEnd;
        }
        return index + value;
    }

    private long reverse(long index, long size) {
        return size - 1 - index;
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
