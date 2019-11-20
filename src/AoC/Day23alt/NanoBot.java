package AoC.Day23alt;

import java.util.Objects;

public class NanoBot implements Comparable<NanoBot> {
    private static int numberOfBots = 0;
    private Point point;
    private int radius;
    private int id;
    private int numberOfIntersections;

    public NanoBot(int x, int y, int z, int radius) {
        this.radius = radius;
        point = new Point(x, y, z);
        id = numberOfBots;
        numberOfBots++;
    }

    public void setNumberOfIntersections() {
        for (NanoBot bot : Day23.nanoBots) {
            if (intersects(bot)) {
                numberOfIntersections++;
            }
        }
    }

    public NanoBot(Point point, int radius) {
        this.point = point;
        this.radius = radius;
        id = numberOfBots;
        numberOfBots++;
    }

    public Point getPoint() {
        return point;
    }

    public boolean intersects(NanoBot other) {
        return distance(other) < radius + other.radius;
    }

    public int getX() {
        return point.getX();
    }

    public int getY() {
        return point.getY();
    }

    public int getZ() {
        return point.getZ();
    }

    private int distance(NanoBot other) {
        int x = Math.abs(getX() - other.getX());
        int y = Math.abs(getY() - other.getY());
        int z = Math.abs(getZ() - other.getZ());
        return x + y + z;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NanoBot nanoBot = (NanoBot) o;
        return radius == nanoBot.radius &&
                point.equals(nanoBot.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point, radius);
    }

    public int getRadius() {
        return radius;
    }

    @Override
    public String toString() {
        return "NanoBot " + id + "\tIntersections: " + numberOfIntersections;
    }

    @Override
    public int compareTo(NanoBot o) {
        return o.numberOfIntersections - numberOfIntersections;
    }
}
