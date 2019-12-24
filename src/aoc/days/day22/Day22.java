package aoc.days.day22;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.math.Tuple;

import java.util.*;

public class Day22 extends Day {
    private List<Tuple<Technique, Long>> instructions;

    public static void main(String[] args) {
        new Day22();
    }

    @Override
    protected void part1() {
        long size = 10007;
        Deck deck = new Deck(size);
        for (Tuple<Technique, Long> instruction : instructions) {
            switch (instruction.getFirst()) {
                case DEAL:
                    deck.dealIntoNewStack();
                    break;
                case CUT:
                    deck.cut(instruction.getSecond());
                    break;
                case DEALINC:
                    deck.dealWithIncrement(instruction.getSecond());
                    break;
                default:
                    throw new InputMismatchException("Unsupported instruction " + instruction.getFirst());
            }
        }
        System.out.println(deck.indexOf(2019));
    }

    @Override
    protected void part2() {
        System.out.println();
        long size = 119315717514047L;
        //long index = 2020;
        int cycleLength = 0;
        for (long index = 0; index < size; index++) {
            long afterIndex = index;
            for (int i = 0; i < instructions.size(); i++) {
                Tuple<Technique, Long> instruction = instructions.get(i);
                switch (instruction.getFirst()) {
                    case DEAL:
                        afterIndex = dealIntoNewStack(index, size);
                        //index = beforeDealIntoNewStack(index, size);
                        break;
                    case CUT:
                        afterIndex = cut(index, size, instruction.getSecond());
                        //index = beforeCut(index, size, instruction.getSecond());
                        break;
                    case DEALINC:
                        afterIndex = dealWithIncrement(index, size, instruction.getSecond());
                        //index = beforeDealWithIncrement(index, size, instruction.getSecond());
                        break;
                    default:
                        throw new InputMismatchException("Unsupported instruction " + instruction.getFirst());
                }
            }
            if (afterIndex == 2020) {
                System.out.println(index);
                break;
            }
        }/*
        while (true) {
            System.out.println(cycleLength);
            //System.out.println(index);
            for (int i = 0; i < instructions.size(); i++) {
                Tuple<Technique, Long> instruction = instructions.get(i);
                switch (instruction.getFirst()) {
                    case DEAL:
                        index = dealIntoNewStack(index, size);
                        //index = beforeDealIntoNewStack(index, size);
                        break;
                    case CUT:
                        index = cut(index, size, instruction.getSecond());
                        //index = beforeCut(index, size, instruction.getSecond());
                        break;
                    case DEALINC:
                        index = dealWithIncrement(index, size, instruction.getSecond());
                        //index = beforeDealWithIncrement(index, size, instruction.getSecond());
                        break;
                    default:
                        throw new InputMismatchException("Unsupported instruction " + instruction.getFirst());
                }
            }
            if (index == 2020) {
                break;
            }
            cycleLength++;
        }
        System.out.println(cycleLength);*/
    }

    private long dealWithIncrement(long index, long size, long n) {
        return (index * n) % size;
    }

    private long cut(long index, long size, long n) {
        if (n > 0) {
            if (index < n) {
                return size - 1 - (n - 1 - index);
            } else {
                return index - n;
            }
        } else {
            return cut(index, size, size + n);
        }
    }

    private long dealIntoNewStack(long index, long size) {
        return size - 1 - index;
    }

    private long beforeDealWithIncrement(long index, long size, long n) {
        long oldIndex = 0;
        while (oldIndex % size != index) {
            oldIndex = (oldIndex + n) % size;
        }
        return oldIndex / n;
    }

    private long beforeCut(long index, long size, long n) {
        if (n > 0) {
            if (index >= size - n) {
                return index - (size - n);
            } else {
                return  index + n;
            }
        } else {
            return beforeCut(index, size, size + n);
        }
    }

    private long beforeDealIntoNewStack(long index, long size) {
        return size - 1 - index;
    }

    @Override
    protected void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            if (s.charAt(0) == 'c') {
                long n = InputUtils.getIntsNegative(s).get(0);
                instructions.add(new Tuple<>(Technique.CUT, n));
            } else if (s.charAt(5) == 'i') {
                instructions.add(new Tuple<>(Technique.DEAL, 0L));
            } else {
                long n = InputUtils.getIntsNegative(s).get(0);
                instructions.add(new Tuple<>(Technique.DEALINC, n));
            }
        }
    }
}
