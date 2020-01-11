package aoc.days.day10;

import java.util.List;

public class Output implements Receiver {
    private final List<Integer> outputs;

    public Output(List<Integer> outputs) {
        this.outputs = outputs;
    }

    @Override
    public void receive(int value) {
        outputs.add(value);
    }
}
