package days.day10;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;
import java.util.stream.Collectors;

public class Day10 extends Day {
    List<Integer> joltages;

    public static void main(String[] args) {
        new Day10().runAndPrint();
    }

    @Override
    public long part1() {
        int oneDiffs = 0;
        int threeDiffs = 0;
        for (int i = 1; i < joltages.size(); i++) {
            int diff = joltages.get(i) - joltages.get(i - 1);
            if (diff == 1) {
                oneDiffs++;
            } else if (diff == 3) {
                threeDiffs++;
            }
        }
        return oneDiffs * threeDiffs;
    }

    @Override
    public long part2() {
        return getNCombinations(joltages);
    }

    private long getNCombinations(List<Integer> joltages) {
        Map<Integer, Long> combinationsMap = new HashMap<>();
        return getNCombinations(joltages, 0, combinationsMap);
    }

    private long getNCombinations(List<Integer> joltages, int i, Map<Integer, Long> combinationsMap) {
        if (i == joltages.size() - 1) {
            return 1;
        }
        if (combinationsMap.containsKey(i)) {
            return combinationsMap.get(i);
        }
        long combinations = 0;
        for (int j = 1; j <= 3 && i + j < joltages.size(); j++) {
            if (joltages.get(i + j) - joltages.get(i) <= 3) {
                combinations += getNCombinations(joltages, i + j, combinationsMap);
            } else {
                break;
            }
        }
        combinationsMap.put(i, combinations);
        return combinations;
    }

    @Override
    public void setup() {
        Queue<Integer> pq = new PriorityQueue<>();
        int max = 0;
        for (String s : lines) {
            int n = Integer.parseInt(s);
            if (n > max) {
                max = n;
            }
            pq.add(n);
        }
        pq.add(0);
        pq.add(max + 3);
        joltages = new ArrayList<>();
        while (!pq.isEmpty()) {
            joltages.add(pq.poll());
        }
    }
}
