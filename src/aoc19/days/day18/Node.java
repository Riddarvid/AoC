package aoc19.days.day18;

import aoc19.utils.geometry.Point2D;

import java.util.InputMismatchException;
import java.util.Objects;

public class Node {
    private final Point2D position;
    private final Character door;
    private final Character key;
    private final boolean start;

    public Node(int x, int y) {
        this(new Point2D(x, y));
    }

    public Node(int x, int y, char c) {
        this(new Point2D(x, y), c);
    }

    public Node(Point2D position) {
        this.position = position;
        door = null;
        key = null;
        start = false;
    }

    public Node(Point2D position, char c) {
        this.position = position;
        if (Character.isUpperCase(c)) {
            door = Character.toLowerCase(c);
            key = null;
            start = false;
        } else if (Character.isLowerCase(c)) {
            key = c;
            door = null;
            start = false;
        } else if (c == '@') {
            key = null;
            door = null;
            start = true;
        } else {
            throw new InputMismatchException();
        }
    }

    public int getX() {
        return (int) position.getX();
    }

    public int getY() {
        return (int) position.getY();
    }

    public boolean isStart() {
        return start;
    }

    public Point2D getPosition() {
        return position;
    }

    public Character getDoor() {
        return door;
    }

    public Character getKey() {
        return key;
    }

    @Override
    public String toString() {
        if (key != null) {
            return position.toString() + " Key: " + key;
        }
        if (door != null) {
            return position.toString() + " Door: " + door;
        }
        if (start) {
            return position.toString() + " @";
        }
        return position.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Node node = (Node) o;
        return Objects.equals(position, node.position) &&
                Objects.equals(door, node.door) &&
                Objects.equals(key, node.key) &&
                Objects.equals(start, node.start);
    }

    @Override
    public int hashCode() {
        return Objects.hash(position, door, key, start);
    }

    public boolean isKeyOrDoor() {
        return key != null || door != null;
    }

    public boolean hasKey() {
        return key != null;
    }

    public boolean isDoor() {
        return door != null;
    }

    public boolean isKey() {
        return key != null;
    }

    public boolean isSpecial() {
        return key != null || door != null || start;
    }
}
