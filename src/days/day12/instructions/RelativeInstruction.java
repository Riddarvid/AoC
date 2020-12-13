package days.day12.instructions;

import days.day12.Ship;

public class RelativeInstruction extends Instruction {
    private final RelativeDirection relativeDirection;

    public RelativeInstruction(RelativeDirection relativeDirection, int value) {
        super(value);
        this.relativeDirection = relativeDirection;
    }

    @Override
    public void apply1(Ship ship) {
        switch (relativeDirection) {
            case LEFT -> ship.rotateHeadingLeft(getValue());
            case RIGHT -> ship.rotateHeadingLeft(360 - getValue());
            case FORWARD -> ship.moveForward(getValue());
        }
    }

    @Override
    public void apply2(Ship ship) {
        switch (relativeDirection) {
            case LEFT -> ship.rotateWaypointLeft(getValue());
            case RIGHT -> ship.rotateWaypointLeft(360 - getValue());
            case FORWARD -> ship.moveToWaypoint(getValue());
        }
    }
}
