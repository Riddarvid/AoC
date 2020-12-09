package days.day1;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;

public class Day1 extends Day {
    private List<Integer> entries;
    private Set<Integer> entrySet;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    public long part1() {
        //List<Integer> terms = findTerms(entries, 2, 2020);
        //return product(terms);
        for (int n : entries) {
            if (entries.contains(2020 - n)) {
                return (long) n * (2020 - n);
            }
        }
        throw new InputMismatchException();
    }

    @Override
    public long part2() {
        List<Integer> terms = findTerms(entries, 3, 2020);
        return product(terms);
    }

    //entries needs to be sorted
    private List<Integer> findTerms(List<Integer> entries, int nTerms, int target) {
        entries.sort(Comparator.comparingInt(i -> i));
        return findTerms(entries, 0, nTerms, target, 0);
    }

    private List<Integer> findTerms(List<Integer> entries, int low, int nTerms, int target, int sumSoFar) {
        if (nTerms == 0) {
            if (sumSoFar == target) {
                return new ArrayList<>();
            } else {
                return null;
            }
        }
        for (int i = low; i < entries.size(); i++) {
            if (entries.get(i) + sumSoFar > target) {
                return null;
            }
            List<Integer> candidate = findTerms(entries, i + 1, nTerms - 1, target, sumSoFar + entries.get(i));
            if (candidate != null) {
                candidate.add(entries.get(i));
                return candidate;
            }
        }
        return null;
    }

    private int product(List<Integer> terms) {
        int product = 1;
        for (int n : terms) {
            product *= n;
        }
        return product;
    }

    @Override
    public void setup() {
        entries = ParsingUtils.stringsToIntegers(lines);
        entries.sort(Comparator.comparingInt(i -> i));
        entrySet = new HashSet<>(entries);
    }
}
