package aoc.days.day20;

import aoc.days.Day;
import aoc.days.day20.view.View;
import aoc.utils.geometry.Point2D;

import java.util.*;

public class Day20 extends Day {
    private Map<Point2D, Set<Point2D>> connections;
    private Point2D start;
    private Point2D end;
    private int width;
    private int height;

    public static void main(String[] args) {
        new Day20();
    }

    @Override
    protected void part1() {
        System.out.println(distance(start, end));
    }

    private void search(Set<Point2D> explored, Set<Point2D> foundLastCycle) {
        Set<Point2D> foundThisCycle = new HashSet<>();
        for (Point2D point : foundLastCycle) {
            Set<Point2D> newPoints = connections.get(point);
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

    private int distance(Point2D start, Point2D end) {
        Set<Point2D> explored = new HashSet<>();
        Set<Point2D> foundLastCycle = new HashSet<>();
        explored.add(start);
        foundLastCycle.add(start);
        int distance = 0;
        while (!foundLastCycle.contains(end)) {
            distance++;
            search(explored, foundLastCycle);
        }
        return distance;
    }

    @Override
    protected void part2() {
        System.out.println(distanceWithDepth(start, end));
    }

    private int distanceWithDepth(Point2D start, Point2D end) {
        Set<Location> explored = new HashSet<>();
        Set<Location> foundLastCycle = new HashSet<>();
        Location startLocation = new Location(start, 0);
        Location endLocation = new Location(end, 0);
        explored.add(startLocation);
        foundLastCycle.add(startLocation);
        //View view = new View(explored, foundLastCycle, connections.keySet(), width, height);
        int distance = 0;
        while (!foundLastCycle.contains(endLocation)) {
            /*try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            view.update(explored, foundLastCycle, connections.keySet());*/
            distance++;
            searchWithDepth(explored, foundLastCycle);
        }
        return distance;
    }

    private void searchWithDepth(Set<Location> explored, Set<Location> foundLastCycle) {
        Set<Location> foundThisCycle = new HashSet<>();
        for (Location location : foundLastCycle) {
            Point2D point = location.getPoint();
            Set<Point2D> newPoints = connections.get(point);
            Point2D exit = null;
            if (isPortal(location.getPoint())) {
                exit = getExit(location.getPoint(), connections.keySet());
            }
            for (Point2D found : newPoints) {
                Location newLocation;
                if (exit != null && exit.equals(found)) {
                    if (isInner(location)) {
                        newLocation = new Location(found, location.getDepth() + 1);
                    } else {
                        if (location.getDepth() == 0) {
                            continue;
                        } else {
                            newLocation = new Location(found, location.getDepth() - 1);
                        }
                    }
                } else {
                    newLocation = new Location(found, location.getDepth());
                }
                if (!explored.contains(newLocation)) {
                    explored.add(newLocation);
                    foundThisCycle.add(newLocation);
                }
            }
        }
        foundLastCycle.clear();
        foundLastCycle.addAll(foundThisCycle);
    }

    private boolean isInner(Location location) {
        int x = (int)location.getX();
        int y = (int)location.getY();
        return x > 5 && x < width - 5 && y > 5 && y < height - 5;
    }

    @Override
    protected void setup() {
        height = lines.size();
        width = lines.get(3).length() + 2;
        Set<Point2D> points = new HashSet<>();
        for (int rowIndex = 0; rowIndex < lines.size(); rowIndex++) {
            String row = lines.get(rowIndex);
            for (int pos = 0; pos < row.length(); pos++) {
                if (row.charAt(pos) == '.') {
                    points.add(new Point2D(pos, rowIndex));
                }
            }
        }
        connections = new HashMap<>();
        for (Point2D current : points) {
            Set<Point2D> connected = new HashSet<>();
            Point2D north = new Point2D(current.getX(), current.getY() - 1);
            Point2D south = new Point2D(current.getX(), current.getY() + 1);
            Point2D west = new Point2D(current.getX() - 1, current.getY());
            Point2D east = new Point2D(current.getX() + 1, current.getY());
            if (points.contains(north)) {
                connected.add(north);
            }
            if (points.contains(south)) {
                connected.add(south);
            }
            if (points.contains(west)) {
                connected.add(west);
            }
            if (points.contains(east)) {
                connected.add(east);
            }
            if (isPortal(current)) {
                connected.add(getExit(current, points));
            }
            if (isStartOrEnd(current)) {
                String id = getId(current);
                if (id.equals("AA")) {
                    start = current;
                } else {
                    end = current;
                }
            }
            connections.put(current, connected);
        }
    }

    private boolean isStartOrEnd(Point2D current) {
        int x = (int)current.getX();
        int y = (int)current.getY();
        if (Character.isAlphabetic(lines.get(y + 1).charAt(x)) ||
                Character.isAlphabetic(lines.get(y - 1).charAt(x)) ||
                Character.isAlphabetic(lines.get(y).charAt(x - 1)) ||
                Character.isAlphabetic(lines.get(y).charAt(x + 1))) {
            String id = getId(current);
            return id.equals("AA") || id.equals("ZZ");
        } else {
            return false;
        }
    }

    private Point2D getExit(Point2D current, Set<Point2D> points) {
        String id = getId(current);
        List<Point2D> portalPoints = getPortalPoints(id, points);
        if (portalPoints.get(0).equals(current)) {
            return portalPoints.get(1);
        } else {
            return portalPoints.get(0);
        }
    }

    private String getId(Point2D current) {
        int x = (int)current.getX();
        int y = (int)current.getY();
        String id;
        if (Character.isAlphabetic(lines.get(y - 1).charAt(x))) {
            id = "" + lines.get(y - 2).charAt(x) + lines.get(y - 1).charAt(x);
        } else if (Character.isAlphabetic(lines.get(y + 1).charAt(x))) {
            id = "" + lines.get(y + 1).charAt(x) + lines.get(y + 2).charAt(x);
        } else if (Character.isAlphabetic(lines.get(y).charAt(x - 1))) {
            id = "" + lines.get(y).charAt(x - 2) + lines.get(y).charAt(x - 1);
        } else if (Character.isAlphabetic(lines.get(y).charAt(x + 1))) {
            id = "" + lines.get(y).charAt(x + 1) + lines.get(y).charAt(x + 2);
        } else {
            throw new InputMismatchException("Current is not portal");
        }
        return id;
    }

    private List<Point2D> getPortalPoints(String id, Set<Point2D> points) {
        List<Point2D> portalPoints = new ArrayList<>();
        for (Point2D point : points) {
            if (isPortal(point)) {
                if (getId(point).equals(id)) {
                    portalPoints.add(point);
                }
            }
        }
        if (portalPoints.size() != 2) {
            throw new InputMismatchException();
        }
        return portalPoints;
    }

    private boolean isPortal(Point2D current) {
        int x = (int)current.getX();
        int y = (int)current.getY();
        if (Character.isAlphabetic(lines.get(y + 1).charAt(x)) ||
                Character.isAlphabetic(lines.get(y - 1).charAt(x)) ||
                Character.isAlphabetic(lines.get(y).charAt(x - 1)) ||
                Character.isAlphabetic(lines.get(y).charAt(x + 1))) {
            String id = getId(current);
            return !id.equals("AA") && !id.equals("ZZ");
        } else {
            return false;
        }
    }
}
