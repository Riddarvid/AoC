package aoc.days.Day6;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;

public class Puzzle12 {
    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day6/input.txt"));
        Point[] locs = new Point[input.length];
        for (int i = 0; i < locs.length; i++) {
            locs[i] = new Point(RegEx.getIntegers(input[i]).get(0), RegEx.getIntegers(input[i]).get(1));
        }

        int minX = minX(locs);
        int maxX = maxX(locs);
        int minY = minY(locs);
        int maxY = maxY(locs);
        int n = locs.length;
        int size = 10000;

        Point[] points = new Point[(maxX - minX + 2 * size / n) * (maxY - minY + 2 * size /n)];
        int index = 0;
        for (int x = minX - size/n; x < maxX + size/n; x++) {
            for (int y = minY - size/n; y < maxY + size/n; y++) {
                points[index] = new Point(x, y, locs);
                index++;
            }
        }

        int sum = 0;
        for (Point p : points) {
            if (p.getIsolation() < 10000) {
                sum++;
            }
        }
        System.out.println(sum);
    }




    public static int minX(Point[] points) {
        int lowest = points[0].getX();
        for (Point p : points) {
            lowest = p.getX() < lowest ? p.getX() : lowest;
        }
        return lowest;
    }

    public static int maxX(Point[] points) {
        int highest = points[0].getY();
        for (Point p : points) {
            highest = p.getX() > highest ? p.getX() : highest;
        }
        return highest;
    }

    public static int minY(Point[] points) {
        int lowest = points[0].getY();
        for (Point p : points) {
            lowest = p.getY() < lowest ? p.getY() : lowest;
        }
        return lowest;
    }

    public static int maxY(Point[] points) {
        int highest = points[0].getY();
        for (Point p : points) {
            highest = p.getY() > highest ? p.getY() : highest;
        }
        return highest;
    }

}
