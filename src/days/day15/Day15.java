package days.day15;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day15 extends Day {
    private List<Integer> startingNumbers;

    public static void main(String[] args) {
        new Day15().runAndPrint();
    }

    @Override
    public long part1() {
        return getNumber(2020);
    }

    @Override
    public long part2() {
        return getNumber(30000000);
    }

    private int getNumber(int n) {
        int[] timeStamps = new int[n];
        int time = 1;
        for (; time < startingNumbers.size() + 1; time++) {
            timeStamps[startingNumbers.get(time - 1)] = time;
        }
        int next = 0;
        for (; time < n; time++) {
            if (timeStamps[next] != 0) {
                int newLast = time - timeStamps[next];
                timeStamps[next] = time;
                next = newLast;
            } else {
                timeStamps[next] = time;
                next = 0;
            }
        }
        return next;
    }

    @Override
    public void setup() {
        startingNumbers = ParsingUtils.getIntegers(lines.get(0));
    }
}
