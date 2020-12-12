package days.day12;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day12 extends Day {
    private List<Instruction> instructions;

    public static void main(String[] args) {
        new Day12().runAndPrint();
    }

    @Override
    public long part1() {
        Ship ship = new Ship();
        for (Instruction i : instructions) {
            i.apply1(ship);
        }
        return Math.abs(ship.getX()) + Math.abs(ship.getY());
    }

    @Override
    public long part2() {
        Ship ship = new Ship();
        for (Instruction i : instructions) {
            i.apply2(ship);
        }
        return Math.abs(ship.getX()) + Math.abs(ship.getY());
    }

    @Override
    public void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            char c = s.charAt(0);
            int value = Integer.parseInt(s.substring(1));
            Direction direction;
            switch (c) {
                case 'E' -> direction = Direction.EAST;
                case 'S' -> direction = Direction.SOUTH;
                case 'W' -> direction = Direction.WEST;
                case 'N' -> direction = Direction.NORTH;
                case 'R' -> direction = Direction.RIGHT;
                case 'L' -> direction = Direction.LEFT;
                case 'F' -> direction = Direction.FORWARD;
                default -> {throw new InputMismatchException();}
            }
            instructions.add(new Instruction(direction, value));
        }
    }
}
