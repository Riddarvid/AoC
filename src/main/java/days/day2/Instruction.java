package days.day2;

public class Instruction {
    private final Direction direction;
    private final int distance;

    public Instruction(Direction direction, int distance) {
        this.direction = direction;
        this.distance = distance;
    }

    public Direction getDirection() {
        return direction;
    }

    public int getDistance() {
        return distance;
    }
}
