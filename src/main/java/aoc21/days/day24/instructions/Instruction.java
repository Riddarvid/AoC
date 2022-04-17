package aoc21.days.day24.instructions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public abstract class Instruction {
    protected final List<String> parameters;

    protected Instruction(List<String> parameters) {
        this.parameters = parameters;
    }

    public abstract void execute(Map<Character, Long> registers, Queue<Integer> inputQueue);

    protected boolean isImmediate(String parameter) {
        return !Character.isAlphabetic(parameter.charAt(0));
    }

    protected long valueOf(String parameter, Map<Character, Long> registers) {
        if (isImmediate(parameter)) {
            return Long.parseLong(parameter);
        }
        return registers.get(parameter.charAt(0));
    }

    public static Instruction parse(String instructionString) {
        String[] tokens = instructionString.split(" ");
        List<String> parameters = new ArrayList<>();
        if (tokens[0].equals("inp")) {
            parameters.add(tokens[1]);
            return new Input(parameters);
        }
        parameters.add(tokens[1]);
        parameters.add(tokens[2]);
        switch (tokens[0]) {
            case "add" -> {
                return new Add(parameters);
            }
            case "mul" -> {
                return new Multiply(parameters);
            }
            case "div" -> {
                return new Divide(parameters);
            }
            case "mod" -> {
                return new Modulo(parameters);
            }
            case "eql" -> {
                return new Equals(parameters);
            }
        }
        throw new IllegalArgumentException("No such instruction: " + tokens[0]);
    }
}
