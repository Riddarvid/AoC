package aoc.days.day13;

public class Scanner {
    private int position;
    private final int range;
    private Direction direction;

    public Scanner(int range) {
        position = 0;
        this.range = range;
        direction = Direction.DOWN;
    }
    public void move() {
        if (direction == Direction.DOWN) {
            position++;
            if (position == range - 1) {
                direction = Direction.UP;
            }
        } else {
            position--;
            if (position == 0) {
                direction = Direction.DOWN;
            }
        }
    }

    public int getPosition() {
        return position;
    }

    public int getRange() {
        return range;
    }

    public void resetPosition() {
        position = 0;
        direction = Direction.DOWN;
    }
}
