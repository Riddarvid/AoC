package aoc.days.day22;

import aoc.days.Day;
import aoc.utils.geometry.Point2D;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day22 extends Day {
    private Map<Point2D, Node> nodes;
    private int maxX;
    private int maxY;

    public static void main(String[] args) {
        new Day22();
    }

    @Override
    protected void part1() {
        System.out.println(viable(nodes));
    }

    private String mapToString(Map<Point2D, Node> nodes, int maxX, int maxY) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i <= maxY; i++) {
            for (int j = 0; j <= maxX; j++) {
                if (i == 0 && j == maxX) {
                    sb.append('G');
                    continue;
                }
                if (i == 0 && j == 0) {
                    sb.append('S');
                    continue;
                }
                Node node = nodes.get(new Point2D(j, i));
                double used = node.getUsed();
                double size = node.getSize();
                if (used / size < 0.1) {
                    sb.append('_');
                } else if (size > 100) {
                    sb.append('#');
                } else {
                    sb.append('.');
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    private int viable(Map<Point2D, Node> nodes) {
        int sum = 0;
        List<Node> nodeList = new ArrayList<>(nodes.values());
        for (int i = 0; i < nodeList.size(); i++) {
            Node a = nodeList.get(i);
            for (int j = 0; j < nodeList.size(); j++) {
                if (i == j) {
                    continue;
                }
                Node b = nodeList.get(j);
                if (a.getUsed() > 0 && a.fitsIn(b)) {
                    sum++;
                }
            }
        }
        return sum;
    }

    @Override
    protected void part2() {
        System.out.println(mapToString(nodes, maxX, maxY));
        System.out.println(shortest(nodes, maxX, maxY));
    }

    private int shortest(Map<Point2D, Node> nodes, int maxX, int maxY) {
        Point2D wallEnd = getWallEnd(nodes);
        Point2D emptyNode = getEmpty(nodes);
        int distance = 0;
        distance += emptyNode.getX() - wallEnd.getX() + 1;
        distance += emptyNode.getY();
        distance += maxX - wallEnd.getX();
        distance += 1;
        distance += (maxX - 1) * 5;
        return distance;
    }

    private Point2D getEmpty(Map<Point2D, Node> nodes) {
        for (Point2D point : nodes.keySet()) {
            if (nodes.get(point).isEmpty()) {
                return point;
            }
        }
        throw new InputMismatchException("No empty node");
    }

    private Point2D getWallEnd(Map<Point2D, Node> nodes) {
        Point2D wallEnd = null;
        for (Point2D point : nodes.keySet()) {
            if (nodes.get(point).isWall()) {
                if (wallEnd == null || point.getX() < wallEnd.getX()) {
                    wallEnd = point;
                }
            }
        }
        return wallEnd;
    }

    @Override
    protected void setup() {
        nodes = new HashMap<>();
        for (int i = 2; i <lines.size(); i++) {
            String s = lines.get(i);
            List<Integer> values = InputUtils.getInts(s);
            nodes.put(new Point2D(values.get(0), values.get(1)), new Node(values.get(2), values.get(3)));
        }
        String s = lines.get(lines.size() - 1);
        List<Integer> values = InputUtils.getInts(s);
        maxX = values.get(0);
        maxY = values.get(1);
    }
}
