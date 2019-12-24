package aoc.days.day18;

import aoc.days.Day;
import aoc.utils.geometry.Point2D;

import java.util.*;

public class Day18 extends Day {
    private Set<Point2D> points;
    private Set<Point2D> keys;
    private Set<Point2D> doors;
    private Map<Point2D, Point2D> lockedBy;
    private Point2D start;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        //Map<DijkstraPoint, Integer> distances = dijkstra(start, points, keys, doors);
        //System.out.println(getShortest(distances));
        System.out.println(shortestDistance(start, points, keys, doors));
    }

    private int shortestDistance(Point2D start, Set<Point2D> points, Set<Point2D> keys, Set<Point2D> doors) {
        DijkstraPoint startDijkstra = new DijkstraPoint(start, new HashSet<>());
        Set<DijkstraPoint> foundLastCycle = new HashSet<>();
        Set<DijkstraPoint> foundTwoCycles = new HashSet<>();
        foundLastCycle.add(startDijkstra);
        foundTwoCycles.add(startDijkstra);
        int distance = 0;
        while (!isDone(foundLastCycle)) {
            distance++;
            search(foundTwoCycles, foundLastCycle, points);
        }
        return distance;
    }

    private boolean isDone(Set<DijkstraPoint> foundLastCycle) {
        for (DijkstraPoint dp : foundLastCycle) {
            if (dp.nKeys() == keys.size()) {
                return true;
            }
        }
        return false;
    }

    private void search(Set<DijkstraPoint> foundTwoCycles, Set<DijkstraPoint> foundLastCycle, Set<Point2D> validPoints) {
        Set<DijkstraPoint> foundThisCycle = new HashSet<>();
        for (DijkstraPoint current : foundLastCycle) {
            Set<DijkstraPoint> neighbours = getNeighbours(current, foundLastCycle, foundTwoCycles, validPoints);
            foundThisCycle.addAll(neighbours);
        }
        foundTwoCycles.clear();
        foundTwoCycles.addAll(foundLastCycle);
        foundLastCycle.clear();
        foundLastCycle.addAll(foundThisCycle);
    }

    private Set<DijkstraPoint> getNeighbours(DijkstraPoint current, Set<DijkstraPoint> foundLastCycle, Set<DijkstraPoint> foundTwoCycles, Set<Point2D> validPoints) {
        Set<Point2D> neighbours = getNeighbours(current, validPoints);
        Set<DijkstraPoint> dijkstraNeighbours = new HashSet<>();
        for (Point2D neighbour : neighbours) {
            Set<Point2D> currentKeys = current.getKeys();
            if (keys.contains(neighbour)) {
                currentKeys.add(neighbour);
            }
            DijkstraPoint dp = new DijkstraPoint(neighbour, currentKeys);
            if (!foundLastCycle.contains(dp) && !foundTwoCycles.contains(dp)) {
                if (!doors.contains(neighbour) || dp.hasKey(lockedBy.get(neighbour))) {
                    dijkstraNeighbours.add(dp);
                }
            }
        }
        return dijkstraNeighbours;
    }

    private int getShortest(Map<DijkstraPoint, Integer> distances) {
        int lowest = Integer.MAX_VALUE;
        for (DijkstraPoint dp : distances.keySet()) {
            if (distances.get(dp) < lowest) {
                lowest = distances.get(dp);
            }
        }
        return lowest;
    }

    private Map<DijkstraPoint, Integer> dijkstra(Point2D start, HashSet<Point2D> points, HashSet<Point2D> keys, HashSet<Point2D> doors) {
        DijkstraPoint startDijkstra = new DijkstraPoint(start, new HashSet<>());
        Map<DijkstraPoint, Integer> distances = new HashMap<>();
        PriorityQueue<DijkstraPoint> toVisit = new PriorityQueue<>(new DijkstraComparator(distances));
        toVisit.add(startDijkstra);
        distances.put(startDijkstra, 0);
        HashSet<DijkstraPoint> visited = new HashSet<>();
        while (!toVisit.isEmpty()) {
            DijkstraPoint current = toVisit.poll();
            visited.add(current);
            Set<DijkstraPoint> neighbours = getNeighbours(current, points, keys, toVisit, visited, doors);
            for (DijkstraPoint neighbour : neighbours) {
                int oldDistance = distances.getOrDefault(neighbour, Integer.MAX_VALUE);
                int newDistance = distances.get(current) + 1;
                if (newDistance < oldDistance) {
                    distances.put(neighbour, newDistance);
                }
                if (!toVisit.contains(neighbour)) {
                    toVisit.add(neighbour);
                }
            }
        }
        Map<DijkstraPoint, Integer> keyPoints = new HashMap<>();
        for (DijkstraPoint dp : distances.keySet()) {
            if (keys.contains(dp.getPoint()) && dp.nKeys() == keys.size()) {
                keyPoints.put(dp, distances.get(dp));
            }
        }
        return keyPoints;
    }

    private Set<DijkstraPoint> getNeighbours(DijkstraPoint current, HashSet<Point2D> validPoints, HashSet<Point2D> keys, PriorityQueue<DijkstraPoint> toVisit, HashSet<DijkstraPoint> visited, Set<Point2D> doors) {
        Set<Point2D> neighbours = getNeighbours(current, validPoints);
        Set<DijkstraPoint> dps = new HashSet<>();
        for (Point2D neighbour : neighbours) {
            Set<Point2D> currentKeys = current.getKeys();
            if (keys.contains(neighbour)) {
                currentKeys.add(neighbour);
            }
            DijkstraPoint dijkstraNeighbour = new DijkstraPoint(neighbour, currentKeys);
            if (!toVisit.contains(dijkstraNeighbour) && !visited.contains(dijkstraNeighbour)) {
                if (!doors.contains(neighbour) || dijkstraNeighbour.hasKey(lockedBy.get(neighbour))) {
                    dps.add(dijkstraNeighbour);
                }
            }
        }
        return dps;
    }

    private Set<Point2D> getNeighbours(DijkstraPoint current, Set<Point2D> validPoints) {
        Set<Point2D> neighbours = new HashSet<>();
        Point2D north = current.getPoint().moveBy(0, -1);
        Point2D south = current.getPoint().moveBy(0, 1);
        Point2D west = current.getPoint().moveBy(-1, 0);
        Point2D east = current.getPoint().moveBy(1, 0);
        if (validPoints.contains(north)) {
            neighbours.add(north);
        }
        if (validPoints.contains(south)) {
            neighbours.add(south);
        }
        if (validPoints.contains(west)) {
            neighbours.add(west);
        }
        if (validPoints.contains(east)) {
            neighbours.add(east);
        }
        return neighbours;
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        points = new HashSet<>();
        keys = new HashSet<>();
        doors = new HashSet<>();
        lockedBy = new HashMap<>();
        Map<Point2D, Character> doorToChar = new HashMap<>();
        Map<Character, Point2D> charToKey = new HashMap<>();
        for (int rowIndex = 0; rowIndex < lines.size(); rowIndex++) {
            String row = lines.get(rowIndex);
            for (int pos = 0; pos < row.length(); pos++) {
                char c = row.charAt(pos);
                if (c != '#') {
                    Point2D point = new Point2D(pos, rowIndex);
                    points.add(point);
                    if (Character.isLowerCase(c)) {
                        charToKey.put(c, point);
                        keys.add(point);
                    } else if (Character.isUpperCase(c)) {
                        doorToChar.put(point, c);
                        doors.add(point);
                    } else if (c == '@') {
                        start = point;
                    } else if (c != '.') {
                        throw new InputMismatchException("" + c);
                    }
                }
            }
        }
        for (Point2D door : doors) {
            char c = doorToChar.get(door);
            c = Character.toLowerCase(c);
            Point2D key = charToKey.get(c);
            lockedBy.put(door, key);
        }
    }
}