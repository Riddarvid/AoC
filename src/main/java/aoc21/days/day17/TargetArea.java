package aoc21.days.day17;

import aoc.math.geometry.Point;

public class TargetArea {
    private final Point bottomLeft;
    private final Point topRight;

    public TargetArea(int x1, int x2, int y1, int y2) {
        int highX = Math.max(x1, x2);
        int lowX = Math.min(x1, x2);
        int highY = Math.max(y1, y2);
        int lowY = Math.min(y1, y2);
        this.bottomLeft = new Point(lowX, lowY);
        this.topRight = new Point(highX, highY);
    }

    public boolean contains(Probe probe) {
        Point point = probe.getPosition();
        return bottomLeft.getX() <= point.getX() && point.getX() <= topRight.getX() &&
                bottomLeft.getY() <= point.getY() && point.getY() <= topRight.getY();
    }

    public boolean hasBeenPassed(Probe probe) {
        return hasBeenPassedX(probe) || hasBeenPassedY(probe);
    }

    private boolean hasBeenPassedX(Probe probe) {
        return (probe.getDX() > 0 && probe.getX() > topRight.getX()) || (probe.getDX() < 0 && probe.getX() < bottomLeft.getX());
    }

    private boolean hasBeenPassedY(Probe probe) {
        return probe.getY() < bottomLeft.getY();
    }

    public int getLowX() {
        return bottomLeft.getX();
    }

    public int getHighX() {
        return topRight.getX();
    }

    public int getLowY() {
        return bottomLeft.getY();
    }

    public int getHighY() {
        return topRight.getY();
    }
}
