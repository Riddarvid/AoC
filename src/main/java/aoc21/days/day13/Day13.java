package aoc21.days.day13;

import aoc.days.Day;
import aoc.math.geometry.Point;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day13 extends Day {
    private Set<Point> points;
    private List<Instruction> instructions;

    @Override
    public void setup() {
        points = new HashSet<>();
        instructions = new ArrayList<>();
        int i = 0;
        String line = lines.get(0);
        while (!line.equals("")) {
            String[] coordinates = line.split(",");
            int x = Integer.parseInt(coordinates[0]);
            int y = Integer.parseInt(coordinates[1]);
            points.add(new Point(x, y));
            i++;
            line = lines.get(i);
        }
        i++;
        for (; i < lines.size(); i++) {
            line = lines.get(i);
            String[] tokens = line.split(" ");
            tokens = tokens[2].split("=");
            Direction direction;
            if (tokens[0].equals("x")) {
                direction = Direction.HORIZONTAL;
            } else {
                direction = Direction.VERTICAL;
            }
            int value = Integer.parseInt(tokens[1]);
            instructions.add(new Instruction(direction, value));
        }
    }

    private Set<Point> executeFold(Set<Point> points, Instruction instruction) {
        Set<Point> newPoints = new HashSet<>();
        Direction direction = instruction.getDirection();
        int value = instruction.getValue();
        for (Point point : points) {
            if (direction == Direction.HORIZONTAL) {
                if (point.getX() < value) {
                    newPoints.add(point);
                } else {
                    int distanceToFold = point.getX() - value;
                    newPoints.add(point.moveBy(-distanceToFold * 2, 0));
                }
            } else {
                if (point.getY() < value) {
                    newPoints.add(point);
                } else {
                    int distanceToFold = point.getY() - value;
                    newPoints.add(point.moveBy(0, -distanceToFold * 2));
                }
            }
        }
        return newPoints;
    }

    @Override
    public long part1() {
        Set<Point> afterFirstFold = executeFold(points, instructions.get(0));
        return afterFirstFold.size();
    }

    @Override
    public long part2() {
        Set<Point> folded = points;
        for (Instruction instruction : instructions) {
            folded = executeFold(folded, instruction);
        }
        printPoints(folded);
        return 0;
    }

    private void printPoints(Set<Point> points) {
        Point maxCoordinate = getMaxCoordinate(points);
        for (int y = 0; y <= maxCoordinate.getY(); y++) {
            System.out.println();
            for (int x = 0; x <= maxCoordinate.getX(); x++) {
                if (points.contains(new Point(x, y))) {
                    System.out.print('#');
                } else {
                    System.out.print(' ');
                }
            }
        }
        System.out.println();
    }

    private Point getMaxCoordinate(Set<Point> points) {
        int maxX = 0;
        int maxY = 0;
        for (Point point : points) {
            if (point.getX() > maxX) {
                maxX = point.getX();
            }
            if (point.getY() > maxY) {
                maxY = point.getY();
            }
        }
        return new Point(maxX, maxY);
    }
}
