package aoc.days.Day23;

import java.util.List;

public class NanoBot {
    private Point point;
    private int radius;

    public NanoBot(Point point, int radius) {
        this.point = point;
        this.radius = radius;
    }

    public Point getPoint() {
        return point;
    }

    public int getRadius() {
        return radius;
    }

    public void setRadius(int radius) {
        this.radius = radius;
    }

    @Override
    public String toString() {
        return point.toString() + " radius: " + radius;
    }

    public void updateRange(List<Point> points) {
        for (int a = point.x - radius; a < point.x + radius; a++) {
            for (int b = point.y - radius + Math.abs(a); b < point.y + radius - Math.abs(a); b++) {
                Point p = new Point(a, b, point.z + (radius - a - b));
                if (points.contains(p)) {
                    points.get(points.indexOf(p)).incBotsInRange();
                } else {
                    points.add(p);
                    p.incBotsInRange();
                }
            }
        }
    }
}
