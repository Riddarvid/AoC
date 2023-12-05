package days.day12;

import days.day12.instructions.*;
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
        return ship.getPosition().manhattanDistanceTo();
    }

    @Override
    public long part2() {
        Ship ship = new Ship();
        for (Instruction i : instructions) {
            i.apply2(ship);
        }
        return ship.getPosition().manhattanDistanceTo();
    }

    @Override
    public void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            char c = s.charAt(0);
            int value = Integer.parseInt(s.substring(1));
            Instruction instruction;
            switch (c) {
                case 'E' -> instruction = new CardinalInstruction(CardinalDirection.EAST, value);
                case 'S' -> instruction = new CardinalInstruction(CardinalDirection.SOUTH, value);
                case 'W' -> instruction = new CardinalInstruction(CardinalDirection.WEST, value);
                case 'N' -> instruction = new CardinalInstruction(CardinalDirection.NORTH, value);
                case 'R' -> instruction = new RelativeInstruction(RelativeDirection.RIGHT, value);
                case 'L' -> instruction = new RelativeInstruction(RelativeDirection.LEFT, value);
                case 'F' -> instruction = new RelativeInstruction(RelativeDirection.FORWARD, value);
                default -> {throw new InputMismatchException();}
            }
            instructions.add(instruction);
        }
    }
}
