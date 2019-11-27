package aoc.days.day20;

import java.util.Objects;

public class Vector {
    private final int x;
    private final int y;
    private final int z;

    public Vector(int x, int y, int z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public Vector add(Vector other) {
        return new Vector(x + other.x, y + other.y, z + other.z);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getZ() {
        return z;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vector vector = (Vector) o;
        return x == vector.x &&
                y == vector.y &&
                z == vector.z;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, z);
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + "," + z + ")";
    }
}
