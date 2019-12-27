package aoc.days.day17;

import aoc.days.Day;
import aoc.utils.geometry.Direction;
import aoc.utils.geometry.Point2D;
import aoc.utils.geometry.Vector2D;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.Communicator;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;
import aoc.utils.output.OutputUtils;

import java.util.*;

public class Day17 extends Day implements Controller {
    private long[] memory;
    private Map<Point2D, Character> map;
    private int row;
    private int pos;

    public static void main(String[] args) {
        new Day17();
    }

    @Override
    protected void part1() {
        List<Point2D> intersections = getIntersections();
        int sum = 0;
        for (Point2D point : intersections) {
            sum += point.getX() * point.getY();
        }
        System.out.println(sum);
    }

    private List<Point2D> getIntersections() {
        List<Point2D> intersections = new ArrayList<>();
        for (Point2D point : map.keySet()) {
            if (isIntersection(point)) {
                intersections.add(point);
            }
        }
        return intersections;
    }

    private boolean isIntersection(Point2D point) {
        return map.containsKey(point.moveBy(-1, 0)) &&
                map.containsKey(point.moveBy(1, 0)) &&
                map.containsKey(point.moveBy(0, -1)) &&
                map.containsKey(point.moveBy(0, 1));
    }

    @Override
    protected void part2() {
        Direction direction;
        Point2D position = null;
        for (Point2D point : map.keySet()) {
            if (map.get(point) != '#') {
                position = point;
                break;
            }
        }
        if (position == null) {
            throw new InputMismatchException();
        }
        switch (map.get(position)) {
            case '^':
                direction = Direction.NORTH;
                break;
            case 'v':
                direction = Direction.SOUTH;
                break;
            case '<':
                direction = Direction.WEST;
                break;
            case '>':
                direction = Direction.EAST;
                break;
            default:
                throw new InputMismatchException();
        }
        List<Character> originalPath = getPath(direction, position);
        List<Character> a = null;
        List<Character> b = null;
        List<Character> c = null;
        for (int aLength = 1; aLength <= 10; aLength++) {
            List<Character> path = new ArrayList<>(originalPath);
            a = getSubPath(path, aLength);
            List<Character> pathWithoutA = new ArrayList<>(path);
            while (isSequenceAtBeginning(pathWithoutA, a)) {
                pathWithoutA = removeSequenceFirst(pathWithoutA, a);
            }
            for (int bLength = 1; bLength <= 10; bLength++) {
                b = getSubPath(pathWithoutA, bLength);
                List<Character> pathWithoutB = new ArrayList<>(pathWithoutA);
                while (isSequenceAtBeginning(pathWithoutB, b) || isSequenceAtBeginning(pathWithoutB, a)) {
                    if (isSequenceAtBeginning(pathWithoutB, b)) {
                        pathWithoutB = removeSequenceFirst(pathWithoutB, b);
                    }
                    if (isSequenceAtBeginning(pathWithoutB, a)) {
                        pathWithoutB = removeSequenceFirst(pathWithoutB, a);
                    }
                }
                for (int cLength = 1; cLength <= 10; cLength++) {
                    c = getSubPath(pathWithoutB, cLength);
                    if (isValidABC(originalPath, a, b, c)) {
                        break;
                    }
                }
                if (isValidABC(originalPath, a, b, c)) {
                    break;
                }
            }
            if (isValidABC(originalPath, a, b, c)) {
                break;
            }
        }
        List<Character> mainRoutine = getMainRoutine(originalPath, a, b, c);
        printPath(originalPath);
        printPath(a);
        printPath(b);
        printPath(c);
        printPath(mainRoutine);
        memory[0] = 2;
        Communicator cm = new ScaffoldingCommunicator(memory);
        Thread t = new Thread(cm);
        t.start();
        inputSequence(mainRoutine, cm);
        inputSequence(a, cm);
        inputSequence(b, cm);
        inputSequence(c, cm);
        cm.makeRequest('y');
        cm.makeRequest('\n');
    }

    private void inputSequence(List<Character> sequence, Communicator cm) {
        for (int i = 0; i < sequence.size() - 1; i++) {
            char current = sequence.get(i);
            char next = sequence.get(i + 1);
            cm.makeRequest(current);
            if (Character.isAlphabetic(current) || Character.isAlphabetic(next)) {
                cm.makeRequest(',');
            }
        }
        cm.makeRequest(sequence.get(sequence.size() - 1));
        cm.makeRequest('\n');
    }

    private boolean isValidABC(List<Character> originalPath, List<Character> a, List<Character> b, List<Character> c) {
        if (originalPath.isEmpty()) {
            return true;
        }
        List<Character> path;
        boolean aFirstValid = false;
        boolean bFirstValid = false;
        boolean cFirstValid = false;
        if (isSequenceAtBeginning(originalPath, a)) {
            path = removeSequenceFirst(originalPath, a);
            aFirstValid = isValidABC(path, a, b, c);
        }
        if (isSequenceAtBeginning(originalPath, b)) {
            path = removeSequenceFirst(originalPath, b);
            bFirstValid = isValidABC(path, a, b, c);
        }
        if (isSequenceAtBeginning(originalPath, c)) {
            path = removeSequenceFirst(originalPath, c);
            cFirstValid = isValidABC(path, a, b, c);
        }
        return aFirstValid || bFirstValid || cFirstValid;
    }

    private List<Character> removeSequenceFirst(List<Character> originalPath, List<Character> a) {
        List<Character> path = new ArrayList<>(originalPath);
        if (isSequenceAtBeginning(path, a)) {
            for (int i = 0; i < a.size(); i++) {
                path.remove(0);
            }
            return path;
        }
        throw new InputMismatchException("Subsequence not at beginning");
    }

    private boolean isSequenceAtBeginning(List<Character> path, List<Character> subPath) {
        if (subPath.size() > path.size()) {
            return false;
        }
        return getSubPath(path, subPath.size()).equals(subPath);
    }

    private List<Character> getMainRoutine(List<Character> path, List<Character> a, List<Character> b, List<Character> c) {
        int i = 0;
        List<Character> mainRoutine = new ArrayList<>();
        while (i < path.size()) {
            if (getSubPath(path, i, i + a.size()).equals(a)) {
                mainRoutine.add('A');
                i += a.size();
            } else if (getSubPath(path, i, i + b.size()).equals(b)) {
                mainRoutine.add('B');
                i += b.size();
            } else if (getSubPath(path, i, i + c.size()).equals(c)) {
                mainRoutine.add('C');
                i += c.size();
            } else {
                throw new InputMismatchException();
            }
        }
        return mainRoutine;
    }

    private List<Character> getSubPath(List<Character> path, int beginIndex, int endIndex) {
        if (endIndex > path.size()) {
            endIndex = path.size();
        }
        List<Character> subPath = new ArrayList<>();
        for (int i = beginIndex; i < endIndex; i++) {
            subPath.add(path.get(i));
        }
        return subPath;
    }

    private List<Character> getSubPath(List<Character> path, int endIndex) {
        return getSubPath(path, 0, endIndex);
    }

    private void printPath(List<Character> path) {
        for (int i = 0; i < path.size(); i++) {
            char c = path.get(i);
            System.out.print(c);
        }
        System.out.println();
    }

    private List<Character> getPath(Direction startingDirection, Point2D position) {
        List<Character> path = new ArrayList<>();
        Vector2D direction = new Vector2D(startingDirection);
        while (true) {
            if (map.containsKey(position.moveBy(direction))) {
                int length = 0;
                while (map.containsKey(position.moveBy(direction))) {
                    length++;
                    position = position.moveBy(direction);
                }
                for (char c : Integer.toString(length).toCharArray()) {
                    path.add(c);
                }
            } else if (map.containsKey(position.moveBy(turnRight(direction)))) {
                direction = turnRight(direction);
                path.add('R');
            } else if (map.containsKey(position.moveBy(turnLeft(direction)))) {
                direction = turnLeft(direction);
                path.add('L');
            } else {
                return path;
            }
            //path.add(',');
        }
    }

    private Vector2D turnRight(Vector2D vector) {
        if (vector.getX() == 1) {
            return new Vector2D(0, 1);
        }
        if (vector.getX() == -1) {
            return new Vector2D(0, -1);
        }
        if (vector.getY() == 1) {
            return new Vector2D(-1, 0);
        }
        if (vector.getY() == -1) {
            return new Vector2D(1, 0);
        }
        throw new InputMismatchException();
    }

    private Vector2D turnLeft(Vector2D vector) {
        return turnRight(turnRight(turnRight(vector)));
    }

    @Override
    protected void setup() {
        row = 0;
        pos = 0;
        map = new HashMap<>();
        memory = InputUtils.readProgram(lines);
        IntcodeComputer ic = new IntcodeComputer(this, memory);
        ic.execute();
        OutputUtils.printMap(map, 50, 50);
    }

    @Override
    public long getInput() {
        return 0;
    }

    @Override
    public void output(long val) {
        if (val == 10) {
            row++;
            pos = 0;
        } else {
            char c = (char)val;
            if (c != '.') {
                map.put(new Point2D(pos, row), c);
            }
            pos++;
        }
    }
}
