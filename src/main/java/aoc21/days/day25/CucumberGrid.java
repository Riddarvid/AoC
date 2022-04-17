package aoc21.days.day25;

import aoc.math.geometry.Point;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class CucumberGrid {
    public static int width;
    public static int height;
    private final Set<Point> eastCucumbers;
    private final Set<Point> southCucumbers;

    public CucumberGrid(Set<Point> eastCucumbers, Set<Point> southCucumbers) {
        this.eastCucumbers = eastCucumbers;
        this.southCucumbers = southCucumbers;
    }

    public CucumberGrid move() {
        Set<Point> newEastCucumbers = new HashSet<>();
        Set<Point> newSouthCucumbers = new HashSet<>();
        for (Point cucumber : eastCucumbers) {
            int newX = cucumber.getX() + 1;
            if (newX == width) {
                newX = 0;
            }
            Point movedCucumber = new Point(newX, cucumber.getY());
            if (eastCucumbers.contains(movedCucumber) || southCucumbers.contains(movedCucumber)) {
                newEastCucumbers.add(cucumber);
            } else {
                newEastCucumbers.add(movedCucumber);
            }
        }
        for (Point cucumber : southCucumbers) {
            int newY = cucumber.getY() + 1;
            if (newY == height) {
                newY = 0;
            }
            Point movedCucumber = new Point(cucumber.getX(), newY);
            if (newEastCucumbers.contains(movedCucumber) || southCucumbers.contains(movedCucumber)) {
                newSouthCucumbers.add(cucumber);
            } else {
                newSouthCucumbers.add(movedCucumber);
            }
        }
        return new CucumberGrid(newEastCucumbers, newSouthCucumbers);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CucumberGrid that = (CucumberGrid) o;
        return Objects.equals(eastCucumbers, that.eastCucumbers) && Objects.equals(southCucumbers, that.southCucumbers);
    }

    @Override
    public int hashCode() {
        return Objects.hash(eastCucumbers, southCucumbers);
    }
}
