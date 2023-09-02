package aoc.utils.geometry;

public enum Direction {
    EAST, SOUTH, WEST, NORTH;

    public Direction turn(RelativeDirection relativeDirection) {
        if (relativeDirection == RelativeDirection.RIGHT) {
            return right();
        }
        return left();
    }

    public Direction left() {
        return Direction.values()[(Direction.values().length + this.ordinal() - 1) % Direction.values().length];
    }

    public Direction right() {
        return Direction.values()[(this.ordinal() + 1) % Direction.values().length];
    }
}
