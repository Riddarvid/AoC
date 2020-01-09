package aoc.days.day1;

import aoc.days.Day;
import aoc.utils.geometry.Direction;
import aoc.utils.geometry.Point2D;
import aoc.utils.geometry.RelativeDirection;
import aoc.utils.input.InputUtils;
import aoc.utils.math.Tuple;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Day1 extends Day {
    private List<Tuple<RelativeDirection, Integer>> instructions;

    public static void main(String[] args) {
        new Day1();
    }

    @Override
    protected void part1() {
        Point2D position = new Point2D(0, 0);
        Direction direction = Direction.NORTH;
        for (Tuple<RelativeDirection, Integer> instruction : instructions) {
            direction = direction.turn(instruction.getFirst());
            position = position.moveBy(direction, instruction.getSecond());
        }
        System.out.println(position.distanceFrom(new Point2D(0, 0)));
    }

    @Override
    protected void part2() {
        Point2D position = new Point2D(0, 0);
        HashSet<Point2D> visited = new HashSet<>();
        visited.add(position);
        Direction direction = Direction.NORTH;
        boolean done = false;
        for (Tuple<RelativeDirection, Integer> instruction : instructions) {
            direction = direction.turn(instruction.getFirst());
            for (int i = 0; i < instruction.getSecond(); i++) {
                position = position.moveBy(direction);
                if (visited.contains(position)) {
                    done = true;
                    break;
                }
                visited.add(position);
            }
            if (done) {
                break;
            }
        }
        System.out.println(position.distanceFrom(new Point2D(0, 0)));
    }

    @Override
    protected void setup() {
        List<String> tokens = InputUtils.getTokens(lines.get(0), ',');
        instructions = new ArrayList<>();
        for (String s : tokens) {
            s = s.replace(" ", "");
            RelativeDirection direction;
            if (s.charAt(0) == 'R') {
                direction = RelativeDirection.RIGHT;
            } else {
                direction = RelativeDirection.LEFT;
            }
            int distance = InputUtils.getInts(s).get(0);
            instructions.add(new Tuple<>(direction, distance));
        }
    }
}
