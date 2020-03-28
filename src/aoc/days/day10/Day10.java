package aoc.days.day10;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day10 extends Day {
    private Map<Integer, Robot> robots;
    private Map<Integer, Output> outputs;

    public static void main(String[] args) {
        new Day10();
    }

    @Override
    protected void part1() {
        boolean done = false;
        while (!done) {
            for (int id : robots.keySet()) {
                if (robots.get(id).execute()) {
                    done = true;
                    break;
                }
            }
        }
    }

    @Override
    protected void part2() {
        reset(false);
        while (outputs.get(0).isEmpty() || outputs.get(1).isEmpty() || outputs.get(2).isEmpty()) {
            for (int id : robots.keySet()) {
                robots.get(id).execute();
            }
        }
        System.out.println(outputs.get(0).getNext() * outputs.get(1).getNext() * outputs.get(2).getNext());
    }

    private void reset(boolean part1) {
        robots = new HashMap<>();
        outputs = new HashMap<>();
        //Create robots + outputs
        for (String s : lines) {
            if (s.charAt(0) == 'b') {
                List<String> tokens = InputUtils.getTokens(s, ' ');
                List<Integer> values = InputUtils.getInts(s);
                int id = values.get(0);
                addRobot(id, part1);
                id = values.get(1);
                if (tokens.get(5).equals("output")) {
                    addOutput(id);
                } else {
                    addRobot(id, part1);
                }
                id = values.get(2);
                if (tokens.get(10).equals("output")) {
                    addOutput(id);
                } else {
                    addRobot(id, part1);
                }
                Container low;
                Container high;
                if (tokens.get(5).equals("output")) {
                    low = outputs.get(values.get(1));
                } else {
                    low = robots.get(values.get(1));
                }
                if (tokens.get(10).equals("output")) {
                    high = outputs.get(values.get(2));
                } else {
                    high = robots.get(values.get(2));
                }
                if (tokens.get(3).equals("high")) {
                    Container temp = low;
                    low = high;
                    high = temp;
                }
                robots.get(values.get(0)).setHigh(high);
                robots.get(values.get(0)).setLow(low);
            }
        }
        //Assign initial values
        for (String s : lines) {
            if (s.charAt(0) == 'v') {
                List<Integer> values = InputUtils.getInts(s);
                robots.get(values.get(1)).addChip(values.get(0));
            }
        }
    }

    private void addRobot(int id, boolean part1) {
        if (!robots.containsKey(id)) {
            Robot robot = new Robot(id, part1);
            robots.put(id, robot);
        }
    }

    private void addOutput(int id) {
        if (!outputs.containsKey(id)) {
            Output output = new Output(id);
            outputs.put(id, output);
        }
    }

    @Override
    protected void setup() {
        reset(true);
    }
}
