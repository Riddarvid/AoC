package aoc.days.day25;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Day25 extends Day {
    private Map<Character, State> states;
    private char startState;
    private int steps;

    public static void main(String[] args) {
        new Day25();
    }

    @Override
    protected void part1() {
        int cursor = 0;
        Set<Integer> ones = new HashSet<>();
        State state = states.get(startState);
        for (int i = 0; i < steps; i++) {
            int input;
            if (ones.contains(cursor)) {
                input = 1;
            } else {
                input = 0;
            }
            if (state.toWrite(input) == 1) {
                ones.add(cursor);
            } else {
                ones.remove(cursor);
            }
            cursor += state.toMove(input);
            state = states.get(state.nextState(input));
        }
        System.out.println(ones.size());
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        String row = lines.get(0);
        startState = row.charAt(row.length() - 2);
        row = lines.get(1);
        steps = InputUtilities.getInts(row).get(0);
        states = new HashMap<>();
        for (int i = 3; i < lines.size(); i += 10) {
            addState(states, i);
        }
    }

    private void addState(Map<Character, State> states, int i) {
        String row = lines.get(i);
        char name = getCharFromEnd(row, 1);
        row = lines.get(i + 2);
        int writeIfZero = getCharFromEnd(row, 1) - '0';
        row = lines.get(i + 3);
        int moveIfZero;
        if (getCharFromEnd(row, 2) == 'f') {
            moveIfZero = -1;
        } else {
            moveIfZero = 1;
        }
        row = lines.get(i + 4);
        char nextStateIfZero = getCharFromEnd(row, 1);

        row = lines.get(i + 6);
        int writeIfOne = getCharFromEnd(row, 1) - '0';
        row = lines.get(i + 7);
        int moveIfOne;
        if (getCharFromEnd(row, 2) == 'f') {
            moveIfOne = -1;
        } else {
            moveIfOne = 1;
        }
        row = lines.get(i + 8);
        char nextStateIfOne = getCharFromEnd(row, 1);

        State state = new State(writeIfZero, moveIfZero, nextStateIfZero, writeIfOne, moveIfOne, nextStateIfOne);
        states.put(name, state);
    }

    private char getCharFromEnd(String string, int index) {
        return string.charAt(string.length() - 1 - index);
    }
}
