package aoc21.days.day10;

import aoc.days.Day;

import java.util.*;

public class Day10 extends Day {
    private Map<Character, Character> openingToClosingMap;
    private Map<Character, Integer> errorScoreMap;
    private Map<Character, Integer> autocompleteScoreMap;
    private List<String> incompleteLines;

    @Override
    public void setup() {
        openingToClosingMap = new HashMap<>();
        openingToClosingMap.put('(', ')');
        openingToClosingMap.put('[', ']');
        openingToClosingMap.put('{', '}');
        openingToClosingMap.put('<', '>');

        errorScoreMap = new HashMap<>();
        errorScoreMap.put(')', 3);
        errorScoreMap.put(']', 57);
        errorScoreMap.put('}', 1197);
        errorScoreMap.put('>', 25137);

        autocompleteScoreMap = new HashMap<>();
        autocompleteScoreMap.put(')', 1);
        autocompleteScoreMap.put(']', 2);
        autocompleteScoreMap.put('}', 3);
        autocompleteScoreMap.put('>', 4);

        incompleteLines = new ArrayList<>();
    }

    private long getTotalSyntaxErrorScore(List<String> lines) {
        long sum = 0;
        for (String line : lines) {
            sum += getSyntaxErrorScore(line);
        }
        return sum;
    }

    private long getSyntaxErrorScore(String line) {
        Stack<Character> stack = new Stack<>();
        for (char c : line.toCharArray()) {
            if (openingToClosingMap.containsKey(c)) { //Opening character
                stack.push(c);
                continue;
            }
            if (stack.isEmpty()) { //More closing than opening
                throw new IllegalArgumentException("More closing characters than opening characters.");
            }
            if (openingToClosingMap.get(stack.peek()) == c) { //Matching closing character.
                stack.pop();
                continue;
            }
            return errorScoreMap.get(c);
        }
        if (!stack.isEmpty()) {
            incompleteLines.add(line);
        }
        return 0;
    }

    @Override
    public long part1() {
        return getTotalSyntaxErrorScore(lines);
    }

    private long getTotalAutocompleteScore(List<String> lines) {
        List<Long> scores = new ArrayList<>();
        for (String line : lines) {
            scores.add(getAutocompleteScore(line));
        }
        scores.sort(Long::compareTo);
        return scores.get(scores.size() / 2);
    }

    private long getAutocompleteScore(String line) {
        Stack<Character> unclosedChars = getUnclosedChars(line);
        long score = 0;
        while (!unclosedChars.isEmpty()) {
            score *= 5;
            score += autocompleteScoreMap.get(openingToClosingMap.get(unclosedChars.pop()));
        }
        return score;
    }

    private Stack<Character> getUnclosedChars(String line) {
        Stack<Character> stack = new Stack<>();
        for (char c : line.toCharArray()) {
            if (openingToClosingMap.containsKey(c)) { //Opening character
                stack.push(c);
                continue;
            }
            if (stack.isEmpty()) { //More closing than opening
                throw new IllegalArgumentException("More closing characters than opening characters.");
            }
            if (openingToClosingMap.get(stack.peek()) == c) { //Matching closing character.
                stack.pop();
                continue;
            }
            throw new IllegalArgumentException("Closing char should always match opening char.");
        }
        return stack;
    }

    @Override
    public long part2() {
        return getTotalAutocompleteScore(incompleteLines);
    }
}
