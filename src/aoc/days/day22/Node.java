package aoc.days.day22;

import aoc.utils.Vector;

import java.util.Objects;

public class Node {
    private Vector position;

    public Node(Vector position) {
        this.position = position;
    }

    public Node(int x, int y) {
        position = new Vector(x, y);
    }

    public int getX() {
        return position.getX();
    }

    public int getY() {
        return position.getY();
    }

    public Vector getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Node node = (Node) o;
        return Objects.equals(position, node.position);
    }

    @Override
    public int hashCode() {
        return Objects.hash(position);
    }
}
