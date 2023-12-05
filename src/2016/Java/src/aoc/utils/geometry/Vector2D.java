package aoc.utils.geometry;

import java.util.InputMismatchException;
import java.util.Objects;

public class Vector2D {
    private final long x;
    private final long y;

    public Vector2D(long x, long y) {
        this.x = x;
        this.y = y;
    }

    public Vector2D(Vector2D other) {
        this(other.x, other.y);
    }

    public Vector2D(Direction direction) {
        switch (direction) {
            case SOUTH:
                x = 0;
                y = 1;
                break;
            case NORTH:
                x = 0;
                y = -1;
                break;
            case WEST:
                x = -1;
                y = 0;
                break;
            case EAST:
                x = 1;
                y = 0;
                break;
            default:
                throw new InputMismatchException();
        }
    }

    public double manhattanDistance() {
        double deltaX = Math.abs(x);
        double deltaY = Math.abs(y);
        return deltaX + deltaY ;
    }

    public double length() {
        return Math.sqrt(x * x + y * y);
    }

    public long getX() {
        return x;
    }

    public long getY() {
        return y;
    }

    public Vector2D scale(long scale) {
        return new Vector2D(x * scale, y * scale);
    }

    public Vector2D changeBy(long x, long y) {
        return new Vector2D(this.x + x, this.y + y);
    }

    public Vector2D changeBy(Vector2D other) {
        return new Vector2D(x + other.x, y + other.y);
    }

    public double scalarProduct(Vector2D other) {
        return x * other.x + y * other.y;
    }

    public double angle() {
        return Math.atan2(y, x);
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vector2D vector2D = (Vector2D) o;
        return x == vector2D.x &&
                y == vector2D.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    public Vector2D flipY() {
        return new Vector2D(x, -y);
    }
}
