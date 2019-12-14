package aoc.days.day12;

import aoc.utils.geometry.Point3D;
import aoc.utils.geometry.Vector3D;

import java.util.List;
import java.util.Objects;

public class Moon {
    private Point3D position;
    private Vector3D velocity;

    public Point3D getPosition() {
        return position;
    }

    public Vector3D getVelocity() {
        return velocity;
    }

    public Moon(Point3D point) {
        this.position = new Point3D(point);
        velocity = new Vector3D(0, 0, 0);
    }

    public Moon(long x, long y, long z) {
        this(new Point3D(x, y, z));
    }

    public Moon(Moon moon) {
        position = new Point3D(moon.position);
        velocity = new Vector3D(moon.velocity);
    }

    public void updatePosition() {
        position = position.moveBy(velocity);
    }

    public void updateVelocity(List<Moon> moons) {
        Vector3D velocityChange = new Vector3D(0, 0, 0);
        for (Moon other : moons) {
            long x = -Long.compare(getX(), other.getX());
            long y = -Long.compare(getY(), other.getY());
            long z = -Long.compare(getZ(), other.getZ());
            velocityChange = velocityChange.changeBy(x, y, z);
        }
        velocity = velocity.changeBy(velocityChange);
    }

    private long getX() {
        return position.getX();
    }

    private long getY() {
        return position.getY();
    }

    private long getZ() {
        return position.getZ();
    }

    public long getTotalEnergy() {
        return getPotentialEnergy() * getKineticEnergy();
    }

    private long getKineticEnergy() {
        return velocity.length();
    }

    private long getPotentialEnergy() {
        return position.distanceFrom(new Point3D(0, 0, 0));
    }

    @Override
    public String toString() {
        return position.toString() + ", " + velocity.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Moon moon = (Moon) o;
        return Objects.equals(position, moon.position) &&
                Objects.equals(velocity, moon.velocity);
    }

    @Override
    public int hashCode() {
        return Objects.hash(position, velocity);
    }
}
