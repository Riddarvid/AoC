package days.day24;

import riddarvid.aoc.math.geometry.Point;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

import static days.day24.Direction.*;

public class Instruction {


    private final List<Direction> directions;

    public Instruction(String line) {
        int index = 0;
        directions = new ArrayList<>();
        while (index < line.length()) {
            char c = line.charAt(index);
            if (c == 'e' || c == 'w') {
                directions.add(parseDirection(line.substring(index, index + 1)));
                index += 1;
            } else {
                directions.add(parseDirection(line.substring(index, index + 2)));
                index += 2;
            }
        }
    }

    private Direction parseDirection(String s) {
        switch (s) {
            case "e" -> {return Direction.EAST;}
            case "se" -> {return Direction.SOUTH_EAST;}
            case "sw" -> {return Direction.SOUTH_WEST;}
            case "w" -> {return Direction.WEST;}
            case "nw" -> {return Direction.NORTH_WEST;}
            case "ne" -> {return Direction.NORTH_EAST;}
            default -> throw new InputMismatchException(s);
        }
    }

    public Tile getTile() {
        Point point = new Point(0, 0);
        for (Direction direction : directions) {
            switch (direction) {
                case EAST       -> point = point.moveBy(eastVector);
                case SOUTH_EAST -> point = point.moveBy(southEastVector);
                case SOUTH_WEST -> point = point.moveBy(southWestVector);
                case WEST       -> point = point.moveBy(westVector);
                case NORTH_WEST -> point = point.moveBy(northWestVector);
                case NORTH_EAST -> point = point.moveBy(northEastVector);
                default -> throw new InputMismatchException();
            }
        }
        return new Tile(point);
    }
}
