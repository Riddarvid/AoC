package aoc.days.day23;

import java.util.List;

public class Instruction {
    private Operation operation;
    private final List<String> arguments;

    public Instruction(Operation operation, List<String> arguments) {
        this.operation = operation;
        this.arguments = arguments;
    }

    public Operation getOperation() {
        return operation;
    }

    public List<String> getArguments() {
        return arguments;
    }

    public void toggle() {
        if (arguments.size() == 1) {
            if (operation == Operation.INC) {
                operation = Operation.DEC;
            } else {
                operation = Operation.INC;
            }
        } else if (arguments.size() == 2) {
            if (operation == Operation.JNZ) {
                operation = Operation.CPY;
            } else {
                operation = Operation.JNZ;
            }
        }
    }

    @Override
    public String toString() {
        if (arguments.size() == 1) {
            return operation.toString() + " " + arguments.get(0);
        } else {
            return operation.toString() + " " + arguments.get(0) + " " + arguments.get(1);
        }
    }
}
