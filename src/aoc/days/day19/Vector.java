package aoc.days.day19;

public class Vector {
    private final int x;
    private final int y;

    public Vector(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Vector add(Vector other) {
        return new Vector(x + other.x, y + other.y);
    }

    public Vector scale(int factor) {
        return new Vector(x * factor, y * factor);
    }

    public Vector flip() {
        return new Vector(y, x);
    }

    public Vector invert() {
        return this.scale(-1);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }
}
