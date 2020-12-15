package days.day15;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.List;

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
        int last = startingNumbers.get(startingNumbers.size() - 1);
        int next = 0;
        for (; time <= n; time++) {
            last = next;
            if (timeStamps[next] != 0) {
                int newNext = time - timeStamps[next];
                timeStamps[next] = time;
                next = newNext;
            } else {
                timeStamps[next] = time;
                next = 0;
            }
        }
        return last;
    }

    @Override
    public void setup() {
        startingNumbers = ParsingUtils.getIntegers(lines.get(0));
    }
}
