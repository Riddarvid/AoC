package aoc21.days.day14;

import java.util.HashMap;
import java.util.Map;

public class PolymerGenerator {
    private final Map<Pair, Character> rules;
    private Map<Pair, Long> pairCount;
    private final Map<Character, Long> charCount;

    public PolymerGenerator(Map<Pair, Character> rules, String startingString) {
        this.rules = rules;
        pairCount = new HashMap<>();
        charCount = new HashMap<>();
        for (int i = 0; i < startingString.length() - 1; i++) {
            addPair(new Pair(startingString.charAt(i), startingString.charAt(i + 1)));
            addChar(startingString.charAt(i));
        }
        addChar(startingString.charAt(startingString.length() - 1));
    }

    public void polymerize() {
        Map<Pair, Long> newCounts = new HashMap<>(pairCount);
        for (Pair pair : pairCount.keySet()) {
            if (rules.containsKey(pair)) {
                char product = rules.get(pair);
                long numberOfReactions = pairCount.get(pair);
                addChars(product, numberOfReactions);
                Pair leftProduct = new Pair(pair.getFirstChar(), product);
                Pair rightProduct = new Pair(product, pair.getSecondChar());
                addPairs(newCounts, leftProduct, numberOfReactions);
                addPairs(newCounts, rightProduct, numberOfReactions);
                removePairs(newCounts, pair, numberOfReactions);
            }
        }
        pairCount = newCounts;
    }

    private void removePairs(Map<Pair, Long> newCounts, Pair pair, long numberOfReactions) {
        long oldCount = newCounts.get(pair);
        newCounts.put(pair, oldCount - numberOfReactions);
    }

    private void addPairs(Map<Pair, Long> newCounts, Pair pair, long numberOfReactions) {
        long oldCount = newCounts.getOrDefault(pair, 0L);
        newCounts.put(pair, oldCount + numberOfReactions);
    }

    private void addPair(Pair pair) {
        long count = 0;
        if (pairCount.containsKey(pair)) {
            count = pairCount.get(pair);
        }
        pairCount.put(pair, count + 1);
    }

    private void addChar(char c) {
        addChars(c, 1);
    }

    private void addChars(char c, long count) {
        long oldCount = charCount.getOrDefault(c, 0L);
        charCount.put(c, oldCount + count);
    }

    public Map<Character, Long> getCharacterCounts() {
        return charCount;
    }
}
