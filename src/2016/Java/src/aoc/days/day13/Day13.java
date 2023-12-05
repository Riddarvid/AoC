package aoc.days.day13;

import aoc.days.Day;
import aoc.utils.geometry.Point2D;

import java.util.*;

public class Day13 extends Day {
    private Map<Point2D, Boolean> map;
    private int length;

    public static void main(String[] args) {
        new Day13();
    }

    @Override
    protected void part1() {
        System.out.println(shortest(new Point2D(1, 1), new Point2D(31, 39), map));
    }

    private int shortest(Point2D start, Point2D goal, Map<Point2D, Boolean> map) {
        Set<Point2D> found = new HashSet<>();
        Set<Point2D> toVisit = new HashSet<>();
        found.add(start);
        toVisit.add(start);
        int distance = 0;
        while (!toVisit.isEmpty()) {
            Set<Point2D> toVisitNext = new HashSet<>();
            for (Point2D point : toVisit) {
                if (point.equals(goal)) {
                    return distance;
                }
                Set<Point2D> neighbours = getNeighbours(point, map);
                for (Point2D neighbour : neighbours) {
                    if (!found.contains(neighbour)) {
                        found.add(neighbour);
                        toVisitNext.add(neighbour);
                    }
                }
            }
            distance++;
            toVisit = toVisitNext;
        }
        throw new InputMismatchException("No path exists");
    }

    private int inRange(Point2D start, Map<Point2D, Boolean> map, int range) {
        Set<Point2D> found = new HashSet<>();
        Set<Point2D> toVisit = new HashSet<>();
        found.add(start);
        toVisit.add(start);
        int distance = 0;
        while (distance <= range) {
            Set<Point2D> toVisitNext = new HashSet<>();
            for (Point2D point : toVisit) {
                Set<Point2D> neighbours = getNeighbours(point, map);
                for (Point2D neighbour : neighbours) {
                    if (!found.contains(neighbour)) {
                        found.add(neighbour);
                        toVisitNext.add(neighbour);
                    }
                }
            }
            distance++;
            toVisit = toVisitNext;
        }
        return found.size() - toVisit.size();
    }

    private Set<Point2D> getNeighbours(Point2D point, Map<Point2D, Boolean> map) {
        Set<Point2D> neighbours = new HashSet<>();
        int x = (int)point.getX();
        int y = (int)point.getY();
        if (x > 0) {
            Point2D p = new Point2D(x - 1, y);
            if (map.get(p)) {
                neighbours.add(p);
            }
        }
        if (x < length - 1) {
            Point2D p = new Point2D(x + 1, y);
            if (map.get(p)) {
                neighbours.add(p);
            }
        }
        if (y > 0) {
            Point2D p = new Point2D(x, y - 1);
            if (map.get(p)) {
                neighbours.add(p);
            }
        }
        if (y < length - 1) {
            Point2D p = new Point2D(x, y + 1);
            if (map.get(p)) {
                neighbours.add(p);
            }
        }
        return neighbours;
    }

    @Override
    protected void part2() {
        System.out.println(inRange(new Point2D(1, 1), map, 50));
    }

    @Override
    protected void setup() {
        length = 100;
        int favoriteNumber = Integer.parseInt(lines.get(0));
        map = new HashMap<>();
        for (int x = 0; x < length; x++) {
            for (int y = 0; y < length; y++) {
                map.put(new Point2D(x, y), isOpen(x, y, favoriteNumber));
            }
        }
    }

    private boolean isOpen(int x, int y, int favoriteNumber) {
        int value = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
        int sum = 0;
        while (value > 0) {
            if (value % 2 == 1) {
                sum++;
            }
            value /= 2;
        }
        return sum % 2 == 0;
    }
}
