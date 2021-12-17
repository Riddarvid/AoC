package aoc21.days.day14;

import aoc.days.Day;

import java.util.HashMap;
import java.util.Map;

public class Day14 extends Day {
    private Map<Pair, Character> rules;
    private String startString;

    @Override
    public void setup() {
        startString = lines.get(0);
        rules = new HashMap<>();
        for (int i = 2; i < lines.size(); i++) {
            String[] tokens = lines.get(i).split(" ");
            Pair pair = new Pair(tokens[0].charAt(0), tokens[0].charAt(1));
            rules.put(pair, tokens[2].charAt(0));
        }
    }

    private long getScore(int iterations) {
        PolymerGenerator polymer = new PolymerGenerator(rules, startString);
        for (int i = 0; i < iterations; i++) {
            polymer.polymerize();
        }
        Map<Character, Long> characterCounts = polymer.getCharacterCounts();
        long mostCommonCount = 0;
        long leastCommonCount = Long.MAX_VALUE;
        for (char c : characterCounts.keySet()) {
            long count = characterCounts.get(c);
            if (count > mostCommonCount) {
                mostCommonCount = count;
            }
            if (count < leastCommonCount) {
                leastCommonCount = count;
            }
        }
        return mostCommonCount - leastCommonCount;
    }

    @Override
    public long part1() {
        return getScore(10);
    }

    @Override
    public long part2() {
        return getScore(40);
    }
}
