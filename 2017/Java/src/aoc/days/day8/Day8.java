package aoc.days.day8;

import aoc.utils.InputUtilities;
import aoc.days.Day;
import aoc.days.day8.Instructions.Condition;
import aoc.days.day8.Instructions.Instruction;
import aoc.days.day8.Instructions.conditionOperators.*;
import aoc.days.day8.Instructions.operators.Add;
import aoc.days.day8.Instructions.operators.Operator;
import aoc.days.day8.Instructions.operators.Sub;

import java.util.HashMap;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Map;

public class Day8 extends Day {
    Instruction[] instructions;
    private Map<String, Integer> registers;
    private int largestRegVal = 0;

    public static void main(String[] args) {
        new Day8();
    }

    @Override
    protected void part1() {
        for (Instruction instruction : instructions) {
            instruction.execute(registers);
            int newLargest = getLargest(registers);
            if (newLargest > largestRegVal) {
                largestRegVal = newLargest;
            }
        }
        System.out.println(getLargest(registers));
    }

    private int getLargest(Map<String, Integer> registers) {
        int largest = registers.values().iterator().next();
        for (int value : registers.values()) {
            if (value > largest) {
                largest = value;
            }
        }
        return largest;
    }

    @Override
    protected void part2() {
        System.out.println(largestRegVal);
    }

    @Override
    protected void setup() {
        registers = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            registers.put(tokens.get(0), 0);
            registers.put(tokens.get(4), 0);
        }
        instructions = new Instruction[lines.size()];
        for (int i = 0; i < lines.size(); i++) {
            String s = lines.get(i);
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            Condition condition = new Condition(tokens.get(4), Integer.parseInt(tokens.get(6)), getConditionOperator(tokens.get(5)));
            Operator operator = getOperator(tokens.get(1));
            Instruction instruction = new Instruction(tokens.get(0), Integer.parseInt(tokens.get(2)), operator, condition);
            instructions[i] = instruction;
        }
    }

    private ConditionOperator getConditionOperator(String s) {
        switch (s) {
            case "==":
                return new Equals();
            case "!=":
                return new NotEquals();
            case "<":
                return new LessThan();
            case "<=":
                return new LessOrEqual();
            case ">":
                return new GreaterThan();
            case ">=":
                return new GreaterOrEqual();
        }
        throw new InputMismatchException(s);
    }

    private Operator getOperator(String s) {
        switch (s) {
            case "inc":
                return new Add();
            case "dec":
                return new Sub();
        }
        throw new InputMismatchException(s);
    }
}
