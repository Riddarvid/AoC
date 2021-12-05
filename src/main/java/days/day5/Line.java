package days.day5;

import aoc.math.geometry.Point;

import java.util.ArrayList;
import java.util.List;

public class Line {
    private final List<Point> points = new ArrayList<>();

    public Line(int x1, int y1, int x2, int y2) {
        if (x1 == x2) {
            createVerticalLine(x1, y1, y2);
        } else if (y1 == y2) {
            createHorizontalLine(y1, x1, x2);
        } else {
            createDiagonalLine(x1, y1, x2, y2);
        }
    }

    private void createDiagonalLine(int x1, int y1, int x2, int y2) {
        if (isPositiveSlope(x1, y1, x2, y2)) {
            createPositiveDiagonalLine(x1, y1, x2, y2);
        } else {
            createNegativeDiagonalLine(x1, y1, x2, y2);
        }
    }

    private void createNegativeDiagonalLine(int x1, int y1, int x2, int y2) {
        int lowX = Math.min(x1, x2);
        int highY = Math.max(y1, y2);
        int numberOfPoints = Math.abs(x1 - x2) + 1;
        for (int i = 0; i < numberOfPoints; i++) {
            points.add(new Point(lowX + i, highY - i));
        }
    }

    private void createPositiveDiagonalLine(int x1, int y1, int x2, int y2) {
        int lowX = Math.min(x1, x2);
        int lowY = Math.min(y1, y2);
        int numberOfPoints = Math.abs(x1 - x2) + 1;
        for (int i = 0; i < numberOfPoints; i++) {
            points.add(new Point(lowX + i, lowY + i));
        }
    }

    private boolean isPositiveSlope(int x1, int y1, int x2, int y2) {
        return (x1 < x2 && y1 < y2) || (x1 > x2 && y1 > y2);
    }

    private void createHorizontalLine(int y, int x1, int x2) {
        int lowX = Math.min(x1, x2);
        int highX = Math.max(x1, x2);
        for (int x = lowX; x <= highX; x++) {
            points.add(new Point(x, y));
        }
    }

    private void createVerticalLine(int x, int y1, int y2) {
        int lowY = Math.min(y1, y2);
        int highY = Math.max(y1, y2);
        for (int y = lowY; y <= highY; y++) {
            points.add(new Point(x, y));
        }
    }

    public List<Point> getPoints() {
        return points;
    }
}
