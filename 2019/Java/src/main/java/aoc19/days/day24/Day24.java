package aoc19.days.day24;

import aoc19.utils.geometry.Point2D;
import aoc19.utils.geometry.Point3D;
import riddarvid.aoc.days.Day;

import java.util.*;

public class Day24 extends Day {
    Set<Point3D> bugs;

    public static void main(String[] args) {
        new Day24();
    }

    @Override
    protected void part1() {
        Set<Set<Point3D>> states = new HashSet<>();
        Set<Point3D> state = new HashSet<>(bugs);
        states.add(state);
        while (true) {
            state = generateNext(state);
            if (states.contains(state)) {
                break;
            }
            states.add(state);
        }
        System.out.println(biodiversity(state));
    }

    private int biodiversity(Set<Point3D> state) {
        int sum = 0;
        for (int x = 0; x < 5; x++) {
            for (int y = 0; y < 5; y++) {
                Point3D point = new Point3D(x, y, 0);
                if (state.contains(point)) {
                    sum += Math.pow(2, point.getY() * 5 + point.getX());
                }
            }
        }
        return sum;
    }

    private Set<Point3D> generateNext(Set<Point3D> current) {
        Set<Point3D> next = new HashSet<>();
        Set<Point3D> toVisit = new HashSet<>(current);
        for (Point3D point : current) {
            toVisit.addAll(getNeighbours(point));
        }
        for (Point3D point : toVisit) {
            int sumNeighbours = sumNeighbours(point, current);
            if (current.contains(point)) {
                if (sumNeighbours == 1) {
                    next.add(point);
                }
            } else {
                if (sumNeighbours == 1 || sumNeighbours == 2) {
                    next.add(point);
                }
            }
        }
        return next;
    }

    private int sumNeighbours(Point3D point, Set<Point3D> current) {
        Set<Point3D> neighbours = getNeighbours(point);
        int sum = 0;
        for (Point3D neighbour : neighbours) {
            if (current.contains(neighbour)) {
                sum++;
            }
        }
        return sum;
    }

    private Set<Point3D> getNeighbours(Point3D point) {
        Set<Point2D> neighbours2D = new Point2D(point).neighbours();
        Set<Point3D> neighbours = new HashSet<>();
        for (Point2D point2D : neighbours2D) {
            if (isValid(point2D)) {
                neighbours.add(new Point3D(point2D));
            }
        }
        return neighbours;
    }

    private boolean isValid(Point2D neighbour) {
        return neighbour.getX() >= 0 && neighbour.getX() < 5 && neighbour.getY() >= 0 && neighbour.getY() < 5;
    }

    @Override
    protected void part2() {
        for (int i = 0; i < 200; i++) {
            bugs = generateNextRecursive(bugs);
        }
        System.out.println(bugs.size());
    }

    private Set<Point3D> generateNextRecursive(Set<Point3D> bugs) {
        Set<Point3D> next = new HashSet<>();
        Set<Point3D> toVisit = new HashSet<>(bugs);
        for (Point3D bug : bugs) {
            toVisit.addAll(getNeighboursRecursive(bug));
        }
        for (Point3D point : toVisit) {
            int sumNeighbours = sumNeighboursRecursive(point, bugs);
            if (bugs.contains(point)) {
                if (sumNeighbours == 1) {
                    next.add(point);
                }
            } else {
                if (sumNeighbours == 1 || sumNeighbours == 2) {
                    next.add(point);
                }
            }
        }
        return next;
    }

    private Set<Point3D> getNeighboursRecursive(Point3D bug) {
        int x = (int)bug.getX();
        int y = (int)bug.getY();
        int z = (int)bug.getZ();
        Set<Point3D> neighbours = new HashSet<>();
        switch (x) {
            case 0:
                neighbours.add(new Point3D(1, 2, z - 1));
                neighbours.add(right(bug));
                break;
            case 1:
                neighbours.add(left(bug));
                if (y == 2) {
                    for (int i = 0; i < 5; i++) {
                        neighbours.add(new Point3D(0, i, z + 1));
                    }
                } else {
                    neighbours.add(right(bug));
                }
                break;
            case 2:
                neighbours.add(left(bug));
                neighbours.add(right(bug));
                break;
            case 3:
                neighbours.add(right(bug));
                if (y == 2) {
                    for (int i = 0; i < 5; i++) {
                        neighbours.add(new Point3D(4, i, z + 1));
                    }
                } else {
                    neighbours.add(left(bug));
                }
                break;
            case 4:
                neighbours.add(new Point3D(3, 2, z - 1));
                neighbours.add(left(bug));
                break;
        }
        switch (y) {
            case 0:
                neighbours.add(new Point3D(2, 1, z - 1));
                neighbours.add(down(bug));
                break;
            case 1:
                neighbours.add(up(bug));
                if (x == 2) {
                    for (int i = 0; i < 5; i++) {
                        neighbours.add(new Point3D(i, 0, z + 1));
                    }
                } else {
                    neighbours.add(down(bug));
                }
                break;
            case 2:
                neighbours.add(up(bug));
                neighbours.add(down(bug));
                break;
            case 3:
                neighbours.add(down(bug));
                if (x == 2) {
                    for (int i = 0; i < 5; i++) {
                        neighbours.add(new Point3D(i, 4, z + 1));
                    }
                } else {
                    neighbours.add(up(bug));
                }
                break;
            case 4:
                neighbours.add(new Point3D(2, 3, z - 1));
                neighbours.add(up(bug));
                break;
        }
        return neighbours;
    }

    private Point3D left(Point3D point) {
        return new Point3D(point.getX() - 1, point.getY(), point.getZ());
    }

    private Point3D right(Point3D point) {
        return new Point3D(point.getX() + 1, point.getY(), point.getZ());
    }

    private Point3D up(Point3D point) {
        return new Point3D(point.getX(), point.getY() - 1, point.getZ());
    }

    private Point3D down(Point3D point) {
        return new Point3D(point.getX(), point.getY() + 1, point.getZ());
    }

    private int sumNeighboursRecursive(Point3D bug, Set<Point3D> bugs) {
        Set<Point3D> neighbours = getNeighboursRecursive(bug);
        int sum = 0;
        for (Point3D neighbour : neighbours) {
            if (bugs.contains(neighbour)) {
                sum++;
            }
        }
        return sum;
    }

    @Override
    protected void setup() {
        bugs = new HashSet<>();
        for (int ri = 0; ri < lines.size(); ri++) {
            String row = lines.get(ri);
            for (int pos = 0; pos < row.length(); pos++) {
                Point3D point = new Point3D(pos, ri, 0);
                if (row.charAt(pos) == '#') {
                    bugs.add(point);
                }
            }
        }
    }
}
