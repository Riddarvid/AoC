package aoc.days.day4;

import aoc.FileUtilities;
import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day4 extends Day {
    private List<String> passPhrases;
    public static void main(String[] args) {
        new Day4();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (String passPhrase : passPhrases) {
            if (isValidPart1(passPhrase)) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    private boolean isValidPart1(String passPhrase) {
        List<String> words = FileUtilities.getTokens(passPhrase, ' ');
        List<String> unique = new ArrayList<>();
        for (String word : words) {
            if (unique.contains(word)) {
                return false;
            }
            unique.add(word);
        }
        return true;
    }

    @Override
    protected void part2() {
        int sum = 0;
        for (String passPhrase : passPhrases) {
            if (isValidPart2(passPhrase)) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    private boolean isValidPart2(String passPhrase) {
        List<String> words = FileUtilities.getTokens(passPhrase, ' ');
        for (int i = 0; i < words.size() - 1; i++) {
            for (int j = i + 1; j < words.size(); j++) {
                if (areAnagrams(words.get(i), words.get(j))) {
                    return false;
                }
            }
        }
        return true;
    }

    private boolean areAnagrams(String word1, String word2) {
        if (word1.length() != word2.length()) {
            return false;
        }
        List<Character> word1list = new ArrayList<>();
        List<Character> word2list = new ArrayList<>();
        for (char c : word1.toCharArray()) {
            word1list.add(c);
        }
        for (char c : word2.toCharArray()) {
            word2list.add(c);
        }
        for (Character c : word1list) {
            if (!word2list.contains(c)) {
                return false;
            }
            word2list.remove(c);
        }
        return true;
    }

    @Override
    protected void setup() {
        passPhrases = new ArrayList<>();
        passPhrases.addAll(lines);
    }
}
