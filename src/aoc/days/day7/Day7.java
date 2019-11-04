package aoc.days.day7;

import aoc.utils.InputUtilities;
import aoc.days.Day;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day7 extends Day {
    private Map<String, Program> programs;
    private Program root;

    public static void main(String[] args) {
        new Day7();
    }

    @Override
    protected void part1() {
        for (Program program : programs.values()) {
            if (program.getParent() == null) {
                root = program;
                System.out.println(program.getName());
                break;
            }
        }
    }

    @Override
    protected void part2() {
        Program current = root;
        current.calculateTotalWeight();
        while (!current.isBalanced()) {
            current = current.getUnbalancedChild();
        }
        int expectedWeight = current.getParent().getExpectedWeight();
        int actual = current.getTotalWeight();
        int difference = expectedWeight - actual;
        System.out.println(current.getWeight() + difference);
    }

    @Override
    protected void setup() {
        programs = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            String name = tokens.get(0);
            String weight = tokens.get(1);
            weight = weight.substring(1, weight.length() - 1);
            programs.put(name, new Program(name, Integer.parseInt(weight)));
        }
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            if (tokens.size() > 2) {
                Program parent = programs.get(tokens.get(0));
                for (int i = 3; i < tokens.size(); i++) {
                    String childName = tokens.get(i);
                    childName = childName.replace(",","");
                    Program child = programs.get(childName);
                    parent.addChild(child);
                    child.addParent(parent);
                }
            }
        }
    }
}
