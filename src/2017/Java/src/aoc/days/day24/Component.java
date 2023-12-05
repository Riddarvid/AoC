package aoc.days.day24;

public class Component {
    private final int left;
    private final int right;

    public Component(int left, int right) {
        this.left = left;
        this.right = right;
    }

    public int getLeft() {
        return left;
    }

    public int getRight() {
        return right;
    }

    public int getStrength() {
        return left + right;
    }

    public boolean canConnectTo(int endPins) {
        return left == endPins || right == endPins;
    }
}
