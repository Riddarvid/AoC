package days.day12.instructions;

import days.day12.Ship;

public class CardinalInstruction extends Instruction {
    private final CardinalDirection cardinalDirection;

    public CardinalInstruction(CardinalDirection cardinalDirection, int value) {
        super(value);
        this.cardinalDirection = cardinalDirection;
    }

    @Override
    public void apply1(Ship ship) {
        switch (cardinalDirection) {
            case EAST -> ship.moveBy(getValue(), 0);
            case SOUTH -> ship.moveBy(0, -getValue());
            case WEST -> ship.moveBy(-getValue(), 0);
            case NORTH -> ship.moveBy(0, getValue());
        }
    }

    @Override
    public void apply2(Ship ship) {
        switch (cardinalDirection) {
            case EAST -> ship.moveWaypointBy(getValue(), 0);
            case SOUTH -> ship.moveWaypointBy(0, -getValue());
            case WEST -> ship.moveWaypointBy(-getValue(), 0);
            case NORTH -> ship.moveWaypointBy(0, getValue());
        }
    }
}
