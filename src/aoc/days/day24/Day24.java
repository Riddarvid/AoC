package aoc.days.day24;

import aoc.days.Day;
import aoc.utils.geometry.Point2D;

import java.util.*;

public class Day24 extends Day {
    private Map<Point2D, List<Edge>> edges;
    private Map<Point2D, Integer> pointToKey;
    private Map<Integer, Point2D> keyToPoint;
    private int maxX;
    private int maxY;

    private List<Map<Integer, Integer>> distances;

    private View view;

    public static void main(String[] args) {
        new Day24();
    }

    @Override
    protected void part1() {
        distances = new ArrayList<>();
        for (int i = 0; i < pointToKey.keySet().size(); i++) {
            distances.add(dijkstra(edges, keyToPoint, pointToKey, i));
        }
        System.out.println(length(shortestPath(distances), distances));
    }

    private int length(List<Integer> path, List<Map<Integer, Integer>> distances) {
        int sum = 0;
        for (int i = 0; i < path.size() - 1; i++) {
            int startNode = path.get(i);
            int endNode = path.get(i + 1);
            sum += distances.get(startNode).get(endNode);
        }
        return sum;
    }

    private List<Integer> shortestPath(List<Map<Integer, Integer>> distances) {
        List<Integer> soFar = new ArrayList<>();
        soFar.add(0);
        List<Integer> remaining = new ArrayList<>();
        for (int i = 1; i < distances.size(); i++) {
            remaining.add(i);
        }
        return shortestPath(distances, soFar, remaining);
    }

    private List<Integer> shortestPath(List<Map<Integer, Integer>> distances, List<Integer> soFar, List<Integer> remaining) {
        if (remaining.size() == 1) {
            soFar.add(remaining.get(0));
            return soFar;
        }
        List<Integer> candidateSoFar = new ArrayList<>(soFar);
        List<Integer> candidateRemaining = new ArrayList<>(remaining);
        candidateRemaining.remove(remaining.get(0));
        candidateSoFar.add(remaining.get(0));
        List<Integer> shortest = shortestPath(distances, candidateSoFar, candidateRemaining);
        for (Integer next : remaining) {
            candidateSoFar = new ArrayList<>(soFar);
            candidateRemaining = new ArrayList<>(remaining);
            candidateSoFar.add(next);
            candidateRemaining.remove(next);
            List<Integer> newShortest = shortestPath(distances, candidateSoFar, candidateRemaining);
            if (length(newShortest, distances) < length(shortest, distances)) {
                shortest = newShortest;
            }
        }
        return shortest;
    }

    private List<Integer> shortestPath2(List<Map<Integer, Integer>> distances) {
        List<Integer> soFar = new ArrayList<>();
        soFar.add(0);
        List<Integer> remaining = new ArrayList<>();
        for (int i = 1; i < distances.size(); i++) {
            remaining.add(i);
        }
        return shortestPath2(distances, soFar, remaining);
    }

    private List<Integer> shortestPath2(List<Map<Integer, Integer>> distances, List<Integer> soFar, List<Integer> remaining) {
        if (remaining.size() == 1) {
            soFar.add(remaining.get(0));
            soFar.add(0);
            return soFar;
        }
        List<Integer> candidateSoFar = new ArrayList<>(soFar);
        List<Integer> candidateRemaining = new ArrayList<>(remaining);
        candidateRemaining.remove(remaining.get(0));
        candidateSoFar.add(remaining.get(0));
        List<Integer> shortest = shortestPath2(distances, candidateSoFar, candidateRemaining);
        for (Integer next : remaining) {
            candidateSoFar = new ArrayList<>(soFar);
            candidateRemaining = new ArrayList<>(remaining);
            candidateSoFar.add(next);
            candidateRemaining.remove(next);
            List<Integer> newShortest = shortestPath2(distances, candidateSoFar, candidateRemaining);
            if (length(newShortest, distances) < length(shortest, distances)) {
                shortest = newShortest;
            }
        }
        return shortest;
    }

    private Map<Integer, Integer> dijkstra(Map<Point2D, List<Edge>> edges, Map<Integer, Point2D> keyToPoint, Map<Point2D, Integer> pointToKey, int start) {
        Point2D startPoint = keyToPoint.get(start);
        Map<Point2D, Integer> distances = new HashMap<>();
        Set<Point2D> finished = new HashSet<>();
        Set<Point2D> found = new HashSet<>();
        found.add(startPoint);
        distances.put(startPoint, 0);
        Queue<Point2D> toVisit = new PriorityQueue<>(new NodeComparator(distances));
        toVisit.add(startPoint);
        while (!toVisit.isEmpty()) {
            Point2D current = toVisit.poll();
            finished.add(current);
            for (Edge edge : edges.get(current)) {
                Point2D other = edge.getEnd();
                if (!finished.contains(other)) {
                    distances.put(other, Math.min(distances.getOrDefault(other, Integer.MAX_VALUE), distances.get(current) + edge.getDistance()));
                }
                if (!found.contains(other)) {
                    found.add(other);
                    toVisit.add(other);
                }
            }
        }
        Map<Integer, Integer> keyDistances = new HashMap<>();
        for (Point2D point : pointToKey.keySet()) {
            keyDistances.put(pointToKey.get(point), distances.get(point));
        }
        return keyDistances;
    }

    @Override
    protected void part2() {
        System.out.println(length(shortestPath2(distances), distances));
    }

    @Override
    protected void setup() {
        edges = new HashMap<>();
        pointToKey = new HashMap<>();
        keyToPoint = new HashMap<>();
        maxX = lines.get(0).length() - 1;
        maxY = lines.size() - 1;
        for (int y = 0; y < lines.size(); y++) {
            String row = lines.get(y);
            for (int x = 0; x < row.length(); x++) {
                char c = row.charAt(x);
                if (c != '#') {
                    Point2D point = new Point2D(x, y);
                    edges.put(point, new ArrayList<>());
                    if (c != '.') {
                        int number = c - '0';
                        pointToKey.put(point, number);
                        keyToPoint.put(number, point);
                    }
                }
            }
        }
        for (Point2D point : edges.keySet()) {
            List<Edge> currentEdges = edges.get(point);
            if (edges.containsKey(point.moveBy(-1, 0))) {
                currentEdges.add(new Edge(point, point.moveBy(-1, 0), 1));
            }
            if (edges.containsKey(point.moveBy(1, 0))) {
                currentEdges.add(new Edge(point, point.moveBy(1, 0), 1));
            }
            if (edges.containsKey(point.moveBy(0, -1))) {
                currentEdges.add(new Edge(point, point.moveBy(0, -1), 1));
            }
            if (edges.containsKey(point.moveBy(0, 1))) {
                currentEdges.add(new Edge(point, point.moveBy(0, 1), 1));
            }
        }
        //view = new View(1500, 1000, pointToKey);
        //view.update(edges);
        //printMap(edges, pointToKey);
        compress(edges, keyToPoint, pointToKey);
    }

    private String mapToString(Map<Point2D, List<Edge>> edges, Map<Point2D, Integer> pointToKey) {
        StringBuilder sb = new StringBuilder();
        for (int y = 0; y <= maxY; y++) {
            for (int x = 0; x <= maxX; x++) {
                Point2D point = new Point2D(x, y);
                if (pointToKey.containsKey(point)) {
                    sb.append(pointToKey.get(point));
                } else if (edges.containsKey(point)) {
                    sb.append('.');
                } else {
                    sb.append('#');
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    private void printMap(Map<Point2D, List<Edge>> edges, Map<Point2D, Integer> pointToKey) {
        System.out.println(mapToString(edges, pointToKey));
    }

    private void compress(Map<Point2D, List<Edge>> edges, Map<Integer, Point2D> keyToPoint, Map<Point2D, Integer> pointToKey) {
        int nodesBefore = edges.size();
        Set<Point2D> deadEnds = getDeadEnds(edges, pointToKey); //Except keys
        removeDeadEnds(edges, deadEnds);
        removeCycles(edges);
        /*
        Set<Point2D> duplicates = getDuplicates(edges);*/
        removeMidPoints(edges, pointToKey);
        /*try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        view.update(edges);*/
        if (nodesBefore != edges.size()) {
            compress(edges, keyToPoint, pointToKey);
        }
    }

    private void removeMidPoints(Map<Point2D, List<Edge>> edges, Map<Point2D, Integer> pointToKey) {
        Iterator<Point2D> it = edges.keySet().iterator();
        while (it.hasNext()) {
            Point2D point = it.next();
            if (!pointToKey.containsKey(point)) {
                List<Edge> currentEdges = edges.get(point);
                if (currentEdges.size() != 2) {
                    continue;
                }
                Edge e1 = currentEdges.get(0);
                Edge e2 = currentEdges.get(1);
                int distance = e1.getDistance() + e2.getDistance();
                Point2D o1 = e1.getEnd();
                Point2D o2 = e2.getEnd();
                edges.get(o1).remove(new Edge(o1, point, e1.getDistance()));
                edges.get(o1).add(new Edge(o1, o2, distance));
                edges.get(o2).remove(new Edge(o2, point, e2.getDistance()));
                edges.get(o2).add(new Edge(o2, o1, distance));
                it.remove();
            }
        }
    }

    private void removeCycles(Map<Point2D, List<Edge>> edges) {
        for (Point2D point : edges.keySet()) {
            Set<Edge> toRemove = new HashSet<>();
            List<Edge> currentEdges = edges.get(point);
            for (Edge e : currentEdges) {
                if (e.getStart().equals(e.getEnd())) {
                    toRemove.add(e);
                }
            }
            currentEdges.removeAll(toRemove);
        }
    }

    private void removeDeadEnds(Map<Point2D, List<Edge>> edges, Set<Point2D> deadEnds) {
        for (Point2D point : deadEnds) {
            removeDeadEnd(edges, point);
        }
    }

    private void removeDeadEnd(Map<Point2D, List<Edge>> edges, Point2D point) {
        if (edges.get(point).isEmpty()) {
            edges.remove(point);
            return;
        }
        Edge edge = edges.get(point).get(0);
        Point2D other = edge.getEnd();
        edges.get(other).remove(new Edge(other, point, edge.getDistance()));
        edges.remove(point);
    }

    private Set<Point2D> getDeadEnds(Map<Point2D, List<Edge>> edges, Map<Point2D, Integer> pointToKey) {
        Set<Point2D> deadEnds = new HashSet<>();
        for (Point2D point : edges.keySet()) {
            if (!pointToKey.containsKey(point)) {
                if (edges.get(point).size() < 2) {
                    deadEnds.add(point);
                }
            }
        }
        return deadEnds;
    }
}
