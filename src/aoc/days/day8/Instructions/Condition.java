package aoc.days.day8.Instructions;

import aoc.days.day8.Instructions.conditionOperators.ConditionOperator;

import java.util.Map;

public class Condition {
    private String register;
    private int operand;
    private ConditionOperator operator;

    public Condition(String register, int operand, ConditionOperator operator) {
        this.register = register;
        this.operand = operand;
        this.operator = operator;
    }

    public boolean isValid(Map<String, Integer> registers) {
        return operator.apply(registers.get(register), operand);
    }
}
