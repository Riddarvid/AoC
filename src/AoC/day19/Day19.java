package AoC.day19;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day19 extends Day {
    private String molecule;
    private List<Rule> rules;

    public static void main(String[] args) {
        new Day19();
    }

    @Override
    protected void part1() {
        List<String> uniqueReplacements = new ArrayList<>();
        for (Rule rule : rules) {
            String toBeReplaced = rule.getKey();
            int size = toBeReplaced.length();
            for (int i = 0; i < molecule.length() - size + 1; i++) {
                String test = molecule.substring(i, i + size);
                if (toBeReplaced.equals(test)) {
                    String replacement = replace(molecule, rule.getValue(), i, i + size);
                    if (!uniqueReplacements.contains(replacement)) {
                        uniqueReplacements.add(replacement);
                    }
                }
            }
        }
        System.out.println(uniqueReplacements.size());
    }

    @Override
    protected void part2() {
        /*Queue<Rule> pq = new PriorityQueue<>(rules);
        rules = new ArrayList<>();
        while (!pq.isEmpty()) {
            rules.add(pq.poll());
        }*/
        //int fewest = getFewestAlt("e");
        int fewest = getFewest3(molecule);
        System.out.println(fewest);
    }

    private int getFewest3(String molecule) {
        List<String> uniqueStrings = new ArrayList<>();
        uniqueStrings.add(molecule);
        int steps = 0;
        while (!isDone(uniqueStrings)) {
            steps++;
            System.out.println(steps);
            List<String> next = new ArrayList<>();
            for (String s : uniqueStrings) {
                List<String> current = getReplacements(s);
                for (String string : current) {
                    if (!next.contains(string)) {
                        next.add(string);
                    }
                }
            }
            uniqueStrings = next;
        }
        return steps;
    }

    private List<String> getReplacements(String molecule) {
        List<String> replacements = new ArrayList<>();
        for (Rule rule : rules) {
            String toBeReplaced = rule.getValue();
            int size = toBeReplaced.length();
            for (int i = 0; i < molecule.length() - size + 1; i++) {
                String test = molecule.substring(i, i + size);
                if (toBeReplaced.equals(test)) {
                    String replacement = replace(molecule, rule.getKey(), i, i + size);
                    replacements.add(replacement);
                }
            }
        }
        return replacements;
    }

    private boolean isDone(List<String> uniqueStrings) {
        for (String s : uniqueStrings) {
            if (s.length() == 1) {
                if (s.equals("e")) {
                    return true;
                }
            }
        }
        return false;
    }

    private int getFewestAlt(String molecule) {
        if (molecule.length() == this.molecule.length()) {
            if (molecule.equals(this.molecule)) {
                return 0;
            } else {
                return Integer.MAX_VALUE;
            }
        }
        if (molecule.length() > this.molecule.length()) {
            return Integer.MAX_VALUE;
        }
        int fewest = Integer.MAX_VALUE;
        for (Rule rule : rules) {
            String toBeReplaced = rule.getKey();
            int size = toBeReplaced.length();
            for (int i = 0; i < molecule.length() - size + 1; i++) {
                String current = molecule.substring(i, i + size);
                if (toBeReplaced.equals(current)) {
                    String replacement = replace(molecule, rule.getValue(), i, i + size);
                    int cLength = replacement.length();
                    int potentialFewest = getFewestAlt(replacement);
                    if (potentialFewest < fewest) {
                        fewest = potentialFewest;
                    }
                }
            }
        }
        if (fewest == Integer.MAX_VALUE) {
            return fewest;
        }
        return fewest + 1;
    }

    private int getFewest(String molecule) {
        if (molecule.equals("e")) {
            return 0;
        }
        int fewest = Integer.MAX_VALUE;
        int pLength = molecule.length();
        for (Rule rule : rules) {
            String toBeReplaced = rule.getValue();
            int size = toBeReplaced.length();
            for (int i = 0; i < molecule.length() - size + 1; i++) {
                String current = molecule.substring(i, i + size);
                if (toBeReplaced.equals(current)) {
                    String replacement = replace(molecule, rule.getKey(), i, i + size);
                    int cLength = replacement.length();
                    int potentialFewest = getFewest(replacement);
                    if (potentialFewest < fewest) {
                        fewest = potentialFewest;
                    }
                }
            }
        }
        if (fewest == Integer.MAX_VALUE) {
            return fewest;
        }
        return fewest + 1;
    }

    private String replace(String string, String toInsert, int start, int end) {//start inclusive, end exclusive
        String before = string.substring(0, start);
        String after;
        if (end >= string.length()) {
            after = "";
        } else {
            after = string.substring(end, string.length());
        }
        return before + toInsert + after;
    }

    @Override
    protected void setup() {
        molecule = lines.get(lines.size() - 1);
        lines.remove(lines.size() - 1);
        rules = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            rules.add(new Rule(tokens.get(0), tokens.get(2)));
        }
    }
}
