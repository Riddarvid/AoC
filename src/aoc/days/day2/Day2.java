package aoc.days.day2;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day2 extends Day {
    List<String> ids;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        int doubles = 0;
        int triples = 0;
        for (String id : ids) {
            if (hasRepeats(id, 2)) {
                doubles++;
            }
            if (hasRepeats(id, 3)) {
                triples++;
            }
        }
        System.out.println(doubles * triples);
    }

    private boolean hasRepeats(String id, int repeats) {
        for (char c : id.toCharArray()) {
            if (repeats == getOccurrences(id, c)) {
                return true;
            }
        }
        return false;
    }

    private int getOccurrences(String id, char target) {
        int occurrences = 0;
        for (char c : id.toCharArray()) {
            if (c == target) {
                occurrences++;
            }
        }
        return occurrences;
    }

    @Override
    protected void part2() {
        String id1 = null;
        String id2 = null;
        for (int i = 0; i < ids.size(); i++) {
            String current = ids.get(i);
            if (hasMatch(current, ids.subList(i + 1, ids.size()))) {
                id1 = current;
                id2 = getMatch(current, ids.subList(i + 1, ids.size()));
            }
        }
        System.out.println(findCommon(id1, id2));
    }

    private String findCommon(String id1, String id2) {
        for (int i = 0; i < id1.length(); i++) {
            if (id1.charAt(i) != id2.charAt(i)) {
                return id1.substring(0, i) + id1.substring(i + 1);
            }
        }
        return null;
    }

    private String getMatch(String current, List<String> ids) {
        for (String id : ids) {
            if (matches(current, id)) {
                return id;
            }
        }
        return null;
    }

    private boolean hasMatch(String current, List<String> ids) {
        return getMatch(current, ids) != null;
    }

    private boolean matches(String s1, String s2) {
        int differences = 0;
        for (int i = 0; i < s1.length(); i++) {
            if (s1.charAt(i) != s2.charAt(i)) {
                differences++;
                if (differences > 1) {
                    return false;
                }
            }
        }
        return differences == 1;
    }

    @Override
    protected void setup() {
        ids = new ArrayList<>(lines);
    }
}
