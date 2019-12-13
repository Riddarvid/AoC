package aoc.utils.geometry;

import java.util.Objects;

public class Vector3D {
    private final long x;
    private final long y;
    private final long z;

    public Vector3D(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public Vector3D(Vector3D other) {
        this(other.x, other.y, other.z);
    }

    public long length() {
        long deltaX = Math.abs(x);
        long deltaY = Math.abs(y);
        long deltaZ = Math.abs(z);
        return deltaX + deltaY + deltaZ;
    }

    public long getX() {
        return x;
    }

    public long getY() {
        return y;
    }

    public long getZ() {
        return z;
    }

    public Vector3D changeBy(long x, long y, long z) {
        return new Vector3D(this.x + x, this.y + y, this.z + z);
    }

    public Vector3D changeBy(Vector3D other) {
        return new Vector3D(x + other.x, y + other.y, z + other.z);
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + "," + z + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vector3D vector3D = (Vector3D) o;
        return x == vector3D.x &&
                y == vector3D.y &&
                z == vector3D.z;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, z);
    }
}
