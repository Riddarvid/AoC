package aoc.days.day8.Instructions;

import aoc.days.day8.Instructions.operators.Operator;

import java.util.Map;

public class Instruction {
    private String register;
    private int operand;
    private Operator operator;
    private Condition condition;

    public Instruction(String register, int operand, Operator operator, Condition condition) {
        this.register = register;
        this.operand = operand;
        this.operator = operator;
        this.condition = condition;
    }

    public void execute(Map<String, Integer> registers) {
        if (condition.isValid(registers)) {
            int result = operator.apply(registers.get(register), operand);
            registers.put(register, result);
        }
    }

}
