package aoc19.days.day15;

import aoc19.utils.geometry.Point2D;
import aoc19.utils.geometry.Vector2D;
import aoc19.utils.output.Constants;

import java.util.*;

public class Drone {
    private HashMap<Point2D, Integer> points;
    private Point2D position;
    private Communicator cm;
    private Point2D oxygen;

    public void createMap(long[] memory) {
        cm = new Communicator(memory);
        Thread cmt = new Thread(cm);
        cmt.setDaemon(true);
        cmt.start();
        points = new HashMap<>();
        position = new Point2D(0, 0);
        points.put(position, 1);
        exploreDirection(1);
        exploreDirection(2);
        exploreDirection(3);
        exploreDirection(4);
        removeWalls();
    }

    public void printMap() {
        for (int rowIndex = -25; rowIndex < 25; rowIndex++) {
            for (int pos = -25; pos < 25; pos++) {
                Point2D position = new Point2D(pos, rowIndex);
                if (position.equals(new Point2D(0, 0))) {
                    System.out.print(Constants.ANSI_RED + "X" + Constants.ANSI_RESET);
                } else {
                    int val = points.getOrDefault(position, 0);
                    switch (val) {
                        case 0:
                            System.out.print('#');
                            break;
                        case 1:
                            System.out.print('.');
                            break;
                        case 2:
                            System.out.print(Constants.ANSI_RED + 'O' + Constants.ANSI_RESET);
                            break;
                        default:
                            throw new InputMismatchException("Unsupported value");
                    }
                }
            }
            System.out.println();
        }
    }

    private void removeWalls() {
        List<Point2D> toRemove = new ArrayList<>();
        for (Point2D point : points.keySet()) {
            if (points.get(point) == 0) {
                toRemove.add(point);
            }
        }
        for (Point2D point : toRemove) {
            points.remove(point);
        }
    }

    private void exploreDirection(int direction) {
        Point2D newPosition = position.moveBy(getDirection(direction));
        if (!points.containsKey(newPosition)) {
            cm.makeRequest(direction);
            int response = cm.getStatus();
            points.put(newPosition, response);
            if (response != 0) {
                if (response == 2) {
                    oxygen = newPosition;
                }
                Point2D oldPosition = position;
                position = newPosition;
                exploreDirection(1);
                exploreDirection(2);
                exploreDirection(3);
                exploreDirection(4);
                position = oldPosition;
                cm.makeRequest(oppositeDirection(direction));
                cm.getStatus();
            }
        }
    }

    private int oppositeDirection(int direction) {
        switch (direction) {
            case 1:
                return 2;
            case 2:
                return 1;
            case 3:
                return 4;
            case 4:
                return 3;
            default:
                throw new InputMismatchException("Invalid direction " + direction);
        }
    }

    private Vector2D getDirection(int direction) {
        switch (direction) {
            case 1:
                return new Vector2D(0, 1);
            case 2:
                return new Vector2D(0, -1);
            case 3:
                return new Vector2D(-1, 0);
            case 4:
                return new Vector2D(1, 0);
            default:
                throw new InputMismatchException("Invalid direction " + direction);
        }
    }

    public int findShortestPath() {
        Set<Point2D> explored = new HashSet<>();
        List<Point2D> foundLastCycle = new ArrayList<>();
        explored.add(new Point2D(0, 0));
        foundLastCycle.add(new Point2D(0, 0));
        int distance = 0;
        while (!explored.contains(oxygen)) {
            distance++;
            search(explored, foundLastCycle);
        }
        return distance;
    }

    private void search(Set<Point2D> explored, List<Point2D> foundLastCycle) {
        List<Point2D> foundThisCycle = new ArrayList<>();
        for (Point2D point : foundLastCycle) {
            List<Point2D> newPoints = explore(point);
            for (Point2D found : newPoints) {
                if (!explored.contains(found)) {
                    explored.add(found);
                    foundThisCycle.add(found);
                }
            }
        }
        foundLastCycle.clear();
        foundLastCycle.addAll(foundThisCycle);
    }

    private List<Point2D> explore(Point2D point) {
        List<Point2D> newPoints = new ArrayList<>();
        for (int i = 1; i <= 4; i++) {
            Point2D newPoint = point.moveBy(getDirection(i));
            if (points.containsKey(newPoint)) {
                newPoints.add(newPoint);
            }
        }
        return newPoints;
    }

    public int findOxygenTime() {
        Set<Point2D> explored = new HashSet<>();
        List<Point2D> foundLastCycle = new ArrayList<>();
        explored.add(oxygen);
        foundLastCycle.add(oxygen);
        int distance = 0;
        while (!foundLastCycle.isEmpty()) {
            distance++;
            search(explored, foundLastCycle);
        }
        return distance - 1;
    }
}
