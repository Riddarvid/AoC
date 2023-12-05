package aoc.days.day21;

import java.util.List;

public class Instruction {
    private final Operation operation;
    private final List<String> parameters;

    public Instruction(Operation operation, List<String> parameters) {
        this.operation = operation;
        this.parameters = parameters;
    }

    public Operation getOperation() {
        return operation;
    }

    public List<String> getParameters() {
        return parameters;
    }
}
