package aoc19.utils.geometry;

import java.util.Objects;

public class Point3D {
    private final long x;
    private final long y;
    private final long z;

    public Point3D(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public Point3D(Point3D other) {
        this(other.x, other.y, other.z);
    }

    public Point3D(Point2D point2D) {
        x = point2D.getX();
        y = point2D.getY();
        z = 0;
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

    public long distanceFrom(Point3D other) {
        long deltaX = Math.abs(x - other.x);
        long deltaY = Math.abs(y - other.y);
        long deltaZ = Math.abs(z - other.z);
        return deltaX + deltaY + deltaZ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Point3D point3D = (Point3D) o;
        return x == point3D.x &&
                y == point3D.y &&
                z == point3D.z;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, z);
    }

    @Override
    public String toString() {
        return "(" + x + "," + y + "," + z + ")";
    }

    public Point3D moveBy(long x, long y, long z) {
        return new Point3D(this.x + x, this.y + y, this.z + z);
    }

    public Point3D moveBy(Vector3D vector) {
        return new Point3D(x + vector.getX(), y + vector.getY(), z + vector.getZ());
    }
}
