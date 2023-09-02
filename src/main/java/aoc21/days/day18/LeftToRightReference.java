package aoc21.days.day18;

import java.util.ArrayList;
import java.util.List;

public class LeftToRightReference {
    private final List<RegularNumber> order;
    private int currentIndex;

    public LeftToRightReference() {
        this.order = new ArrayList<>();
        currentIndex = -1;
    }

    public void increaseIndex() {
        currentIndex++;
    }

    public void explode(int firstValue, int secondValue) {
        if (currentIndex >= 0) {
            order.get(currentIndex).add(firstValue);
        }
        if (currentIndex + 3 < order.size()) {
            order.get(currentIndex + 3).add(secondValue);
        }
    }

    public void register(RegularNumber regularNumber) {
        order.add(regularNumber);
    }
}
