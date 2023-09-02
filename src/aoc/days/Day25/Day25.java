package aoc.days.Day25;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Day25 {
    private List<Point> points;

    public static void main(String[] args) {
        new Day25().run();
    }

    private void run() {
        points = initPoints();
        System.out.println("Init done");
        int numberOfConstellations = 0;
        for (Point p : points) {
            if (!p.isSearched()) {
                numberOfConstellations++;
                markConstellation(p);
            }
        }
        System.out.println("Number of constellations: " + numberOfConstellations);
    }

    private void markConstellation(Point p) {
        p.setSearched(true);
        for (Point other : points) {
            if (!other.isSearched()) {
                if (p.isInRangeOf(other)) {
                    markConstellation(other);
                }
            }
        }
    }

    private List<Point> initPoints() {
        List<Point> points = new ArrayList<>();
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day25/input.txt"));
        for (String s : input) {
            List<Integer> values = RegEx.getIntegers(s);
            points.add(new Point(values.get(0), values.get(1), values.get(2), values.get(3)));
        }
        return points;
    }
}
