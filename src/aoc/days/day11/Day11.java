package aoc.days.day11;

import aoc.FileUtilities;
import aoc.days.Day;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day11 extends Day {
    private List<Direction> directions;
    int maxDistance;

    public static void main(String[] args) {
        new Day11();
    }

    @Override
    protected void part1() {
        int x = 0;
        int y = 0;
        maxDistance = 0;
        for (Direction direction : directions) {
            switch (direction) {
                case NW:
                    x--;
                    y++;
                    break;
                case N:
                    y += 2;
                    break;
                case NE:
                    x++;
                    y++;
                    break;
                case SW:
                    x--;
                    y--;
                    break;
                case S:
                    y -= 2;
                    break;
                case SE:
                    x++;
                    y--;
                    break;
                default:
                    throw new InputMismatchException(direction.toString());
            }
            int newDistance = distanceFromOrigo(x, y);
            if (newDistance > maxDistance) {
                maxDistance = newDistance;
            }
        }
        System.out.println(distanceFromOrigo(x, y));
    }

    private int distanceFromOrigo(int x, int y) {
        x = Math.abs(x);
        y = Math.abs(y);
        int distance;
        if (x > y) {
            distance = x;
        } else {
            distance = x + (y - x) / 2;
        }
        return distance;
    }

    @Override
    protected void part2() {
        System.out.println(maxDistance);
    }

    @Override
    protected void setup() {
        String input = lines.get(0);
        List<String> tokens = FileUtilities.getTokens(input, ',');
        directions = new ArrayList<>();
        for (String direction : tokens) {
            switch (direction) {
                case "nw":
                    directions.add(Direction.NW);
                    break;
                case "n":
                    directions.add(Direction.N);
                    break;
                case "ne":
                    directions.add(Direction.NE);
                    break;
                case "sw":
                    directions.add(Direction.SW);
                    break;
                case "s":
                    directions.add(Direction.S);
                    break;
                case "se":
                    directions.add(Direction.SE);
                    break;
                default:
                    throw new InputMismatchException(direction);
            }
        }
    }
}
