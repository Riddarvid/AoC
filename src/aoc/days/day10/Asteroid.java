package aoc.days.day10;

import aoc.utils.geometry.Point2D;
import aoc.utils.geometry.Vector2D;

import java.util.*;

public class Asteroid {
    private final Point2D point;
    private int nLineOfSight;


    public Asteroid(Point2D point) {
        this.point = new Point2D(point);
    }

    public Asteroid(int x, int y) {
        point = new Point2D(x, y);
    }

    public int getNLineOfSight() {
        return nLineOfSight;
    }

    public void calculateNLineOfSight(List<Asteroid> others) {
        List<Double> angles = new ArrayList<>();
        for (Asteroid target : others) {
            angles.add(angleTo(target));
        }
        Set<Double> uniqueAngles = new HashSet<>(angles);
        nLineOfSight = uniqueAngles.size();
    }

    public Double angleTo(Asteroid target) {
        return round(point.vectorTo(target.point).angle());
    }

    public Double angleToUp(Asteroid target) {
        double alpha = point.vectorTo(target.point).flipY().angle() - Math.PI / 2;
        if (alpha < 0) {
            alpha += 2 * Math.PI;
        }
        if (alpha > 0) {
            alpha = 2 * Math.PI - alpha;
        }
        return round(alpha);
    }

    private double round(double value) {
        return Math.round(value * 100000) / 100000.0;
    }

    @Override
    public String toString() {
        return point.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Asteroid asteroid = (Asteroid) o;
        return Objects.equals(point, asteroid.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point);
    }

    public double distanceTo(Asteroid other) {
        return point.vectorTo(other.point).length();
    }

    public long getX() {
        return point.getX();
    }

    public long getY() {
        return point.getY();
    }
}
