package aoc21.days.day9;

import aoc.days.Day;
import aoc.math.geometry.Point;

import java.util.*;

public class Day9 extends Day {
    private int[][] map;
    private List<Point> lowPoints;

    @Override
    public void setup() {
        map = new int[lines.size()][lines.get(0).length()];
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            for (int j = 0; j < line.length(); j++) {
                map[i][j] = line.charAt(j) - '0';
            }
        }
    }

    private boolean isLowPoint(int row, int column, int[][] map) {
        int height = map[row][column];
        if (row > 0 && map[row - 1][column] <= height) {
            return false;
        }
        if (row < map.length - 1 && map[row + 1][column] <= height) {
            return false;
        }
        if (column > 0 && map[row][column - 1] <= height) {
            return false;
        }
        if (column < map[0].length - 1 && map[row][column + 1] <= height) {
            return false;
        }
        return true;
    }

    private List<Point> getLowPoints(int[][] map) {
        List<Point> lowPoints = new ArrayList<>();
        for (int row = 0; row < map.length; row++) {
            for (int column = 0; column < map[row].length; column++) {
                if (isLowPoint(row, column, map)) {
                    lowPoints.add(new Point(column, row));
                }
            }
        }
        return lowPoints;
    }

    private long getRiskLevel(List<Point> lowPoints, int[][] map) {
        long sum = 0;
        for (Point lowPoint : lowPoints) {
            sum += map[lowPoint.getY()][lowPoint.getX()] + 1;
        }
        return sum;
    }

    @Override
    public long part1() {
        lowPoints = getLowPoints(map);
        return getRiskLevel(lowPoints, map);
    }

    @Override
    public long part2() {
        Set<Set<Point>> basins = getBasins(lowPoints, map);
        Queue<Set<Point>> basinQueue = new PriorityQueue<>(new BasinComparator<>());
        basinQueue.addAll(basins);
        long product = 1;
        for (int i = 0; i < 3; i++) {
            product *= basinQueue.poll().size();
        }
        return product;
    }

    private Set<Set<Point>> getBasins(List<Point> lowPoints, int[][] map) {
        Set<Set<Point>> basins = new HashSet<>();
        for (Point lowPoint : lowPoints) {
            basins.add(getBasin(lowPoint, map));
        }
        return basins;
    }

    private Set<Point> getBasin(Point lowPoint, int[][] map) {
        Set<Point> basin = new HashSet<>();
        Set<Point> discovered = new HashSet<>();
        discovered.add(lowPoint);
        Set<Point> discoveredLastRound = new HashSet<>();
        discoveredLastRound.add(lowPoint);
        while (!discoveredLastRound.isEmpty()) {
            Set<Point> discoveredThisRound = new HashSet<>();
            for (Point point : discoveredLastRound) {
                if (point.getY() < 0 || point.getY() >= map.length || point.getX() < 0 || point.getX() >= map[0].length) {
                    continue;
                }
                if (map[point.getY()][point.getX()] != 9) {
                    basin.add(point);
                    Set<Point> neighbours = new HashSet<>();
                    neighbours.add(point.moveBy(0, -1));
                    neighbours.add(point.moveBy(0, 1));
                    neighbours.add(point.moveBy(-1, 0));
                    neighbours.add(point.moveBy(1, 0));
                    for (Point neighbour : neighbours) {
                        if (!discovered.contains(neighbour)) {
                            discovered.add(neighbour);
                            discoveredThisRound.add(neighbour);
                        }
                    }
                }
            }
            discoveredLastRound = discoveredThisRound;
        }
        return basin;
    }
}
