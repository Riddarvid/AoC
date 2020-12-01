package days.day1;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class Day1 extends Day {
    private List<Integer> entries;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    protected void part1() {
        List<Integer> terms = findTerms(entries, 2, 2020);
        System.out.println(product(terms));
    }

    @Override
    protected void part2() {
        List<Integer> terms = findTerms(entries, 3, 2020);
        System.out.println(product(terms));
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
    protected void setup() {
        entries = ParsingUtils.stringsToIntegers(lines);
        entries.sort(Comparator.comparingInt(i -> i));
    }
}
