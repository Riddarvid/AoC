package AoC.Day6;

import AoC.FileUtilities;
import AoC.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Model {
    private List<Point> points = new ArrayList<>();
    private int minX;
    private int minY;

    public Model(int width, int height) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day6/Input.txt"));
        Point[] locs = new Point[input.length];
        for (int i = 0; i < locs.length; i++) {
            locs[i] = new Point(RegEx.getIntegers(input[i]).get(0), RegEx.getIntegers(input[i]).get(1));
        }
        minX = getMiddleX(locs) - width/2;
        minY = getMiddleY(locs) - height/2;
        for (int x = minX; x < minX + width; x++) {
            for (int y = minY; y < minY + height; y++) {
                points.add(new Point(x, y, locs));
            }
        }
    }

    private int getMiddleX(Point[] locs) {
        int minX = locs[0].getX();
        int maxX = minX;
        for (Point p : locs) {
            if (p.getX() < minX) {
                minX = p.getX();
            }
            if (p.getX() > maxX) {
                maxX = p.getX();
            }
        }
        return (minX + maxX)/2;
    }

    private int getMiddleY(Point[] locs) {
        int minY = locs[0].getY();
        int maxY = minY;
        for (Point p : locs) {
            if (p.getY() < minY) {
                minY = p.getY();
            }
            if (p.getY() > maxY) {
                maxY = p.getY();
            }
        }
        return (minY + maxY)/2;
    }

    public List<Point> getPoints() {
        return points;
    }

    public int getMinX() {
        return minX;
    }

    public int getMinY() {
        return minY;
    }
}
