package aoc.days.day18;

import aoc.days.Day;
import aoc.days.day14.Quantity;
import aoc.utils.geometry.Point2D;
import aoc.utils.math.MathUtils;
import aoc.utils.math.Tuple;

import java.util.*;

public class Day18 extends Day {
    private Map<Point2D, Character> map;
    private Map<Point2D, Character> keyToChar;
    private Map<Character, Point2D> charToKey;
    private Map<Point2D, Character> doors;
    private Map<Tuple<Point2D, Point2D>, Integer> distances;
    private Point2D start;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        System.out.println("Yeet");
    }

    private int getDistance(List<Point2D> permutation) {
        int distance = 0;
        for (int i = 0; i < permutation.size() - 1; i++) {
            Tuple<Point2D, Point2D> tuple = new Tuple<>(permutation.get(i), permutation.get(i + 1));
            distance += distances.get(tuple);
        }
        return distance;
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        map = new HashMap<>();
        keyToChar = new HashMap<>();
        charToKey = new HashMap<>();
        doors = new HashMap<>();
        for (int rowIndex = 0; rowIndex < lines.size(); rowIndex++) {
            String row = lines.get(rowIndex);
            for (int pos = 0; pos < row.length(); pos++) {
                char c = row.charAt(pos);
                if (c != '#') {
                    Point2D point = new Point2D(pos, rowIndex);
                    map.put(point, c);
                    if (c != '.') {
                        if (c == '@') {
                            start = point;
                            keyToChar.put(point, c);
                            charToKey.put(c, point);
                        } else if (Character.isUpperCase(row.charAt(pos))) {
                            doors.put(point, c);
                        } else {
                            keyToChar.put(point, c);
                            charToKey.put(c, point);
                        }
                    }
                }
            }
        }
        distances = new HashMap<>();
        for (Point2D point : keyToChar.keySet()) {
            calculateDistances(point);
        }
    }

    private void calculateDistances(Point2D start) {
        DijkstraPoint dijkstraStart = new DijkstraPoint(start);
        Set<DijkstraPoint> visited = new HashSet<>();
        Set<DijkstraPoint> unvisited = new HashSet<>();
        Map<DijkstraPoint, Integer> dijkstraDistances = new HashMap<>();
        dijkstraDistances.put(dijkstraStart, 0);
        Queue<DijkstraPoint> unvisitedQueue = new PriorityQueue<>(new PointComparator(dijkstraDistances));
        unvisited.add(dijkstraStart);
        unvisitedQueue.add(dijkstraStart);
        while (!unvisitedQueue.isEmpty()) {
            DijkstraPoint current = unvisitedQueue.poll();
            unvisited.remove(current);
            visited.add(current);
            Set<DijkstraPoint> neighbours = getUnvisitedNeighbours(current, visited, unvisited, unvisitedQueue);
            for (DijkstraPoint neighbour : neighbours) {
                int newDistance;
                if (doors.containsKey(neighbour.getPoint())) {
                    char door = doors.get(neighbour.getPoint());
                    if (visited.contains(new DijkstraPoint(charToKey.get(Character.toLowerCase(door))))) {
                        newDistance = dijkstraDistances.get(current);
                        if (newDistance != Integer.MAX_VALUE) {
                            newDistance++;
                        }
                    } else {
                        newDistance = Integer.MAX_VALUE;
                    }
                } else {
                    newDistance = dijkstraDistances.get(current);
                    if (newDistance != Integer.MAX_VALUE) {
                        newDistance++;
                    }
                }
                Integer oldDistance = dijkstraDistances.get(neighbour);
                if (oldDistance == null) {
                    oldDistance = Integer.MAX_VALUE;
                    dijkstraDistances.put(neighbour, oldDistance);
                }
                if (newDistance < oldDistance) {
                    dijkstraDistances.put(neighbour, newDistance);
                    neighbour.setPrevious(current);
                }
            }
        }
        for (DijkstraPoint dp : dijkstraDistances.keySet()) {
            if (keyToChar.containsKey(dp.getPoint())) {
                Tuple<Point2D, Point2D> tuple = new Tuple<>(start, dp.getPoint());
                distances.put(tuple, dijkstraDistances.get(dp));
            }
        }
    }

    private List<DijkstraPoint> buildPath(DijkstraPoint point) {
        if (point == null) {
            return new ArrayList<>();
        } else {
            List<DijkstraPoint> path = buildPath(point.getPrevious());
            path.add(point);
            return path;
        }
    }

    private Set<DijkstraPoint> getUnvisitedNeighbours(DijkstraPoint current, Set<DijkstraPoint> visited, Set<DijkstraPoint> unvisited, Queue<DijkstraPoint> unvisitedQueue) {
        DijkstraPoint north = new DijkstraPoint(current.getPoint().moveBy(0, -1));
        DijkstraPoint south = new DijkstraPoint(current.getPoint().moveBy(0, 1));
        DijkstraPoint west = new DijkstraPoint(current.getPoint().moveBy(-1, 0));
        DijkstraPoint east = new DijkstraPoint(current.getPoint().moveBy(1, 0));
        Set<DijkstraPoint> unvisitedNeighbours = new HashSet<>();
        if (map.containsKey(north.getPoint()) && !visited.contains(north)) {
            unvisitedNeighbours.add(north);
            if (!unvisited.contains(north)) {
                unvisited.add(north);
                unvisitedQueue.add(north);
            }
        }
        if (map.containsKey(south.getPoint()) && !visited.contains(south)) {
            unvisitedNeighbours.add(south);
            if (!unvisited.contains(south)) {
                unvisited.add(south);
                unvisitedQueue.add(south);
            }
        }
        if (map.containsKey(west.getPoint()) && !visited.contains(west)) {
            unvisitedNeighbours.add(west);
            if (!unvisited.contains(west)) {
                unvisited.add(west);
                unvisitedQueue.add(west);
            }
        }
        if (map.containsKey(east.getPoint()) && !visited.contains(east)) {
            unvisitedNeighbours.add(east);
            if (!unvisited.contains(east)) {
                unvisited.add(east);
                unvisitedQueue.add(east);
            }
        }
        return unvisitedNeighbours;
    }

    private void search(Set<Point2D> explored, Set<Point2D> foundLastCycle) {
        Set<Point2D> foundThisCycle = new HashSet<>();
        for (Point2D point : foundLastCycle) {
            Set<Point2D> newPoints = getConnections(point);
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

    private Set<Point2D> getConnections(Point2D point) {
        Point2D north = point.moveBy(0, -1);
        Point2D south = point.moveBy(0, 1);
        Point2D west = point.moveBy(-1, 0);
        Point2D east = point.moveBy(1, 0);
        Set<Point2D> connections = new HashSet<>();
        if (map.containsKey(north)) {
            connections.add(north);
        }
        if (map.containsKey(south)) {
            connections.add(south);
        }
        if (map.containsKey(west)) {
            connections.add(west);
        }
        if (map.containsKey(east)) {
            connections.add(east);
        }
        return connections;
    }

    private int getDistance(Point2D start, Point2D end) {
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
}
