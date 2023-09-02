package aoc19.days.day14;

import java.util.List;

public class Requirement {
    private final Quantity output;
    private final List<Quantity> inputs;

    public Requirement(Quantity output, List<Quantity> inputs) {
        this.output = output;
        this.inputs = inputs;
    }

    public Quantity getOutput() {
        return output;
    }

    public List<Quantity> getInputs() {
        return inputs;
    }
}
