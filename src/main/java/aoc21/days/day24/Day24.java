package aoc21.days.day24;

import aoc.days.Day;

import java.util.Comparator;
import java.util.PriorityQueue;

public class Day24 extends Day {
    private ALU alu;

    @Override
    public void setup() {
        alu = new ALU(lines);
    }

    private long findFirstValid(Comparator<State> stateComparator) {
        State startState = new State("", 0L);
        PriorityQueue<State> priorityQueue = new PriorityQueue<>(stateComparator);
        priorityQueue.add(startState);
        while (!priorityQueue.isEmpty()) {
            State current = priorityQueue.poll();
            if (current.inputString().length() == 14) {
                if (current.z() == 0L) {
                    return Long.parseLong(current.inputString());
                }
                continue;
            }
            if (current.z() > Math.pow(26, 14 - current.inputString().length())) {
                continue;
            }
            priorityQueue.addAll(alu.generateStates(current));
        }
        throw new IllegalArgumentException("No valid MONAD number found");
    }

    @Override
    public long part1() {
        return findFirstValid(new LargestComparator());
    }

    @Override
    public long part2() {
        return findFirstValid(new SmallestComparator());
    }
}
