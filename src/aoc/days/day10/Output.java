package aoc.days.day10;

import java.util.LinkedList;

public class Output implements Container {
    private final int id;
    private final LinkedList<Integer> chips;

    public Output(int id) {
        this.id = id;
        chips = new LinkedList<>();
    }

    public boolean isEmpty() {
        return chips.isEmpty();
    }

    public int getNext() {
        return chips.getFirst();
    }

    @Override
    public void addChip(int chip) {
        chips.addLast(chip);
    }
}
