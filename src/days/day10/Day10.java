package days.day10;

import riddarvid.aoc.days.Day;

import java.util.*;

public class Day10 extends Day {
    Set<Integer> joltSet;
    int target;

    public static void main(String[] args) {
        new Day10().runAndPrint();
    }

    @Override
    public long part1() {
        int oneDiffs = 0;
        int threeDiffs = 0;
        int last = 0;
        for (int n = 1; n <= target; n++) {
            if (joltSet.contains(n)) {
                int diff = n - last;
                last = n;
                if (diff == 1) {
                    oneDiffs++;
                } else if (diff == 3) {
                    threeDiffs++;
                }
            }
        }
        return oneDiffs * threeDiffs;
    }

    @Override
    public long part2() {
        return getNCombinations();
    }

    private long getNCombinations() {
        Map<Integer, Long> combinationsMap = new HashMap<>();
        return getNCombinations(0, combinationsMap);
    }

    private long getNCombinations(int n, Map<Integer, Long> combinationsMap) {
        if (n == target) {
            return 1;
        }
        if (combinationsMap.containsKey(n)) {
            return combinationsMap.get(n);
        }
        long combinations = 0;
        for (int j = 1; j <= 3 && n + j <= target; j++) {
            if (joltSet.contains(n + j)) {
                combinations += getNCombinations(n + j, combinationsMap);
            }
        }
        combinationsMap.put(n, combinations);
        return combinations;
    }

    @Override
    public void setup() {
        joltSet = new HashSet<>();
        target = 0;
        for (String s : lines) {
            int n = Integer.parseInt(s);
            if (n > target) {
                target = n;
            }
            joltSet.add(n);
        }
        target += 3;
        joltSet.add(target);
    }
}
