package aoc.days.day10;

import java.util.ArrayList;
import java.util.List;

public class Robot implements Receiver {
    private Receiver lowReceiver;
    private Receiver highReceiver;
    private final int id;
    private final List<Integer> inputs;

    public Robot(int id) {
        inputs = new ArrayList<>();
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public int getLowReceiver() {
        return lowReceiver.getId();
    }

    public int getHighReceiver() {
        return highReceiver.getId();
    }

    public void receive(int value) {
        inputs.add(value);
    }
}
