package aoc.days.day2;

import aoc.days.Day;
import aoc.utils.geometry.Direction;
import aoc.utils.geometry.Point2D;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day2 extends Day {
    private List<List<Direction>> instructions;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        List<Point2D> keys = getKeys1(instructions);
        printKeys1(keys);
    }

    private void printKeys1(List<Point2D> keys) {
        for (Point2D key : keys) {
            int x = (int)key.getX();
            int y = (int)key.getY();
            System.out.print(y * 3 + x + 1);
        }
        System.out.println();
    }

    private List<Point2D> getKeys1(List<List<Direction>> instructions) {
        Point2D position = new Point2D(1, 1); //(1, 1) corresponds to 5 on the part 1 key pad.
        List<Point2D> keys = new ArrayList<>();
        for (List<Direction> instruction : instructions) {
            position = executeInstruction(position, instruction);
            keys.add(position);
        }
        return keys;
    }

    private Point2D executeInstruction(Point2D position, List<Direction> instruction) {
        for (Direction direction : instruction) {
            if (canMove(position, direction)) {
                position = position.moveBy(direction);
            }
        }
        return position;
    }

    private boolean canMove(Point2D position, Direction direction) {
        position = position.moveBy(direction);
        int x = (int)position.getX();
        int y = (int)position.getY();
        return x >= 0 && x <= 2 && y >= 0 && y <= 2;
    }

    @Override
    protected void part2() {
        List<Point2D> keys = getKeys2(instructions);
        printKeys2(keys);
    }

    private void printKeys2(List<Point2D> keys) {
        char[][] keypad = {
                {'ö','ö','1','ö','ö'},
                {'ö','2','3','4','ö'},
                {'5','6','7','8','9'},
                {'ö','A','B','C','ö'},
                {'ö','ö','D','ö','ö'}
        };
        for (Point2D key : keys) {
            int x = (int)key.getX();
            int y = (int)key.getY();
            System.out.print(keypad[y][x]);
        }
        System.out.println();
    }

    private List<Point2D> getKeys2(List<List<Direction>> instructions) {
        Point2D position = new Point2D(0, 2); //(0, 2) corresponds to 5 on the part 2 key pad.
        List<Point2D> keys = new ArrayList<>();
        for (List<Direction> instruction : instructions) {
            position = executeInstruction2(position, instruction);
            keys.add(position);
        }
        return keys;
    }

    private Point2D executeInstruction2(Point2D position, List<Direction> instruction) {
        for (Direction direction : instruction) {
            if (canMove2(position, direction)) {
                position = position.moveBy(direction);
            }
        }
        return position;
    }

    private boolean canMove2(Point2D position, Direction direction) {
        position = position.moveBy(direction);
        int x = (int)position.getX();
        int y = (int)position.getY();
        x -= 2;
        y -= 2;
        x = Math.abs(x);
        y = Math.abs(y);
        return new Point2D(x, y).distanceFrom(new Point2D(0, 0)) < 3;
    }

    @Override
    protected void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            List<Direction> instruction = new ArrayList<>();
            for (char c : s.toCharArray()) {
                switch (c) {
                    case 'R':
                        instruction.add(Direction.EAST);
                        break;
                    case 'D':
                        instruction.add(Direction.SOUTH);
                        break;
                    case 'L':
                        instruction.add(Direction.WEST);
                        break;
                    case 'U':
                        instruction.add(Direction.NORTH);
                        break;
                    default:
                        throw new InputMismatchException("" + c);
                }
            }
            instructions.add(instruction);
        }
    }
}
