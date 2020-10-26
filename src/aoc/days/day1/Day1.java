package aoc.days.day1;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day1 extends Day {
    private List<Integer> frequencyChanges;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    protected void part1() {
        int frequency = 0;
        for (int change : frequencyChanges) {
            frequency += change;
        }
        System.out.println(frequency);
    }

    @Override
    protected void part2() {
        int frequency = 0;
        Set<Integer> found = new HashSet<>();
        int i = 0;
        while (!found.contains(frequency)) {
            found.add(frequency);
            frequency += frequencyChanges.get(i);
            i = (i + 1) % frequencyChanges.size();
        }
        System.out.println(frequency);
    }

    @Override
    protected void setup() {
        frequencyChanges = new ArrayList<>();
        for (String string : lines) {
            frequencyChanges.add(ParsingUtils.getIntegersNegative(string).get(0));
        }
    }
}
