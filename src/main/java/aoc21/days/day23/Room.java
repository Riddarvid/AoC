package aoc21.days.day23;

import java.util.Objects;

public class Room {
    private final int position;
    private final Amphipod outerPosition;
    private final Amphipod innerPosition; //Furthest from the door.

    public Room(int position, Amphipod outerPosition, Amphipod innerPosition) {
        this.position = position;
        this.outerPosition = outerPosition;
        this.innerPosition = innerPosition;
    }

    public int getPosition() {
        return position;
    }

    public Amphipod getOuter() {
        return outerPosition;
    }

    public Amphipod getInner() {
        return innerPosition;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Room room = (Room) o;
        return outerPosition == room.outerPosition && innerPosition == room.innerPosition;
    }

    @Override
    public int hashCode() {
        return Objects.hash(outerPosition, innerPosition);
    }

    public Room copy() {
        return new Room(position, outerPosition, innerPosition);
    }

    public Room push(Amphipod amphipod) {
        if (outerPosition == null && innerPosition == null) {
            return new Room(position, null, amphipod);
        } else if (outerPosition == null) {
            return new Room(position, amphipod, innerPosition);
        } else {
            throw new IllegalArgumentException("Can't push to full room!");
        }
    }

    public Room pop() {
        if (outerPosition != null) {
            return new Room(position, null, innerPosition);
        } else if (innerPosition != null) {
            return new Room(position, null, null);
        } else {
            throw new IllegalArgumentException("Can't pop from empty room!");
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("|");
        if (outerPosition == null) {
            sb.append(".");
        } else {
            sb.append(outerPosition);
        }
        if (innerPosition == null) {
            sb.append(".");
        } else {
            sb.append(innerPosition);
        }
        return sb.toString();
    }
}
