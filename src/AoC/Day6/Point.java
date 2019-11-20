package AoC.Day6;

import java.awt.*;
import java.util.Objects;
import java.util.Random;

public class Point implements Comparable<Point> {
    private final int x;
    private final int y;
    private Point owner;
    private int area = 0;
    private int isolation;
    private Color color;
    private static int colorIndex = 0;
    private static Color[] colors = generateColors();

    private static Color[] generateColors() {
        Color[] colors = new Color[50];
        Random rand = new Random();
        rand.setSeed(System.nanoTime());
        for (int i = 0; i < colors.length; i++) {
            int rand1 = rand.nextInt(256);
            int rand2 = rand.nextInt(256);
            int rand3 = rand.nextInt(256);
            colors[i] = new Color(rand1, rand2, rand3);
        }
        return colors;
    }

    public Point(int x, int y, Point[] locations) {
        this.x = x;
        this.y = y;
        this.owner = getOwner(locations);
        if (owner != null) {
            owner.area++;
            color = owner.color;
        } else {
            color = Color.BLACK;
        }
        isolation = getIsolation(locations);
    }

    private int getIsolation(Point[] points) {
        int sum = 0;
        for (Point p : points) {
            sum += distance(p);
        }
        return sum;
    }

    public Point(int x, int y, Point owner) {
        this.x = x;
        this.y = y;
        this.owner = owner;
    }

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
        color = colors[colorIndex%colors.length];
        colorIndex++;
    }

    private Point getOwner(Point[] locations) {
        Point closest = locations[0];
        int same = 0;
        int comp;
        for (Point loc : locations) {
            comp = distance(loc) - distance(closest);
            if (comp < 0) {
                closest = loc;
                same = 1;
            } else if (comp == 0) {
                same++;
            }
        }
        if (same != 1) {
            return null;
        }
        return closest;
    }

    private int distance(Point p) {
        return (Math.abs(x - p.x) + Math.abs(y - p.y));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Point point = (Point) o;
        return x == point.x &&
                y == point.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    public Point getOwner() {
        return owner;
    }

    @Override
    public String toString() {
        return "{" +
                "Isolation: " + isolation +
                " }";
    }

    @Override
    public int compareTo(Point o) {
        return area - o.area;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getIsolation() {
        return isolation;
    }

    public Color getColor() {
        return color;
    }
}
