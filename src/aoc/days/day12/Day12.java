package aoc.days.day12;

import aoc.utils.InputUtilities;
import aoc.days.Day;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day12 extends Day {
    private Map<Integer, Program> programMap;

    public static void main(String[] args) {
        new Day12();
    }

    @Override
    protected void part1() {
        Program program = programMap.get(0);
        System.out.println(program.getNumberConnected());
        for (int i = 0; i < programMap.size(); i++) {
            programMap.get(i).reset();
        }
    }

    @Override
    protected void part2() {
        int groups = 0;
        for (int i = 0; i < programMap.size(); i++) {
            Program current = programMap.get(i);
            if (!current.isSearched()) {
                groups++;
                current.markGroup();
            }
        }
        System.out.println(groups);
    }

    @Override
    protected void setup() {
        programMap = new HashMap<>();
        for (String s : lines) {
            List<Integer> ints = InputUtilities.getInts(s);
            Program program = new Program(ints.get(0));
            programMap.put(program.getId(), program);
        }
        for (String s : lines) {
            List<Integer> ints = InputUtilities.getInts(s);
            Program program = programMap.get(ints.get(0));
            for (int i = 1; i < ints.size(); i++) {
                program.addConnection(programMap.get(ints.get(i)));
            }
        }
    }
}
