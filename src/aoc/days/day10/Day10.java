package aoc.days.day10;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day10 extends Day {
    private Map<Integer, Robot> robots;

    public static void main(String[] args) {
        new Day10();
    }

    @Override
    protected void part1() {

    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        robots = new HashMap<>();
        for (int i = 0; i < lines.size(); i++) {
            String s = lines.get(i);
            if (s.charAt(0) == 'b') {
                List<Integer> values = InputUtils.getInts(s);
                if (!robots.containsKey(values.get(0))) {
                    Robot robot = new Robot();
                    robots.put(i, robot);
                }
            }
        }
    }
}
