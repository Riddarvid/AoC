package days.day12;

public class Instruction {
    private final Direction direction;
    private final int value;

    public Instruction(Direction direction, int value) {
        this.direction = direction;
        this.value = value;
    }

    public void apply1(Ship ship) {
        switch (direction) {
            case RIGHT -> {
                int current = ship.getDirection().ordinal();
                current = (current + value / 90) % 4;
                ship.setDirection(Direction.values()[current]);
            }
            case LEFT -> {
                int current = ship.getDirection().ordinal();
                current = (current - value / 90 + 4) % 4;
                ship.setDirection(Direction.values()[current]);
            }
            case FORWARD -> moveShip(ship.getDirection(), ship);
            default -> moveShip(direction, ship);
        }
    }

    private void moveShip(Direction direction, Ship ship) {
        switch (direction) {
            case EAST -> ship.move(value, 0);
            case SOUTH -> ship.move(0, -value);
            case WEST -> ship.move(-value, 0);
            case NORTH -> ship.move(0, value);
        }
    }

    public void apply2(Ship ship) {
        switch (direction) {
            case RIGHT -> ship.rotateWaypointLeft(360 - value);
            case LEFT -> ship.rotateWaypointLeft(value);
            case FORWARD -> {
                ship.move(value * ship.getWaypointX(), value * ship.getWaypointY());
            }
            case EAST -> ship.moveWaypoint(value, 0);
            case SOUTH -> ship.moveWaypoint(0, -value);
            case WEST -> ship.moveWaypoint(-value, 0);
            case NORTH -> ship.moveWaypoint(0, value);
        }
    }
}
