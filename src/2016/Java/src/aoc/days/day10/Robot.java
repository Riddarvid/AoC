package aoc.days.day10;

import java.util.LinkedList;

public class Robot implements Container {
    private final int id;
    private Container low;
    private Container high;
    private final boolean part1;
    private final LinkedList<Integer> chips;

    public Robot(int id, boolean part1) {
        this.id = id;
        this.part1 = part1;
        chips = new LinkedList<>();
    }

    @Override
    public void addChip(int chip) {
        chips.addLast(chip);
    }

    public boolean execute() {
        if (chips.size() >= 2) {
            int a = chips.pollFirst();
            int b = chips.pollFirst();
            if (part1) {
                if ((a == 61 && b == 17) || (a == 17 && b == 61)) {
                    System.out.println(id);
                    return true;
                }
            }
            if (a < b) {
                low.addChip(a);
                high.addChip(b);
            } else {
                low.addChip(b);
                high.addChip(a);
            }
            return false;
        } else {
            return false;
        }
    }

    public void setLow(Container low) {
        this.low = low;
    }

    public void setHigh(Container high) {
        this.high = high;
    }
}
