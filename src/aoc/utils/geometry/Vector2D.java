package aoc.utils.geometry;

import java.util.Objects;

public class Vector2D {
    private final double x;
    private final double y;

    public Vector2D(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Vector2D(Vector2D other) {
        this(other.x, other.y);
    }

    public double manhattanLength() {
        double deltaX = Math.abs(x);
        double deltaY = Math.abs(y);
        return deltaX + deltaY ;
    }

    public double length() {
        return Math.sqrt(x * x + y * y);
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public Vector2D scale(double scale) {
        return new Vector2D(x * scale, y * scale);
    }

    public Vector2D changeBy(double x, double y) {
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
