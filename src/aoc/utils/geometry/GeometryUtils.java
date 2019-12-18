package aoc.utils.geometry;

public class GeometryUtils {
    public static Direction turnRight(Direction direction) {
        return Direction.values()[(direction.ordinal() + 1) % Direction.values().length];
    }

    public static Direction turnLeft(Direction direction) {
        return turnRight(turnRight(turnRight(direction)));
    }
}
