package aoc.days.day22;

import aoc.days.Day;
import aoc.utils.Vector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day22 extends Day {
    private List<Node> infected;
    private List<Node> infected2;

    public static void main(String[] args) {
        new Day22();
    }

    @Override
    protected void part1() {
        Direction direction = Direction.NORTH;
        int x = 0;
        int y = 0;
        int infectionBursts = 0;
        for (int i = 0; i < 10000; i++) {
            Node current = new Node(x, y);
            if (infected.contains(current)) {
                direction = turnRight(direction);
                infected.remove(current);
            } else {
                direction = turnLeft(direction);
                infected.add(current);
                infectionBursts++;
            }
            switch (direction) {
                case NORTH:
                    y--;
                    break;
                case EAST:
                    x++;
                    break;
                case SOUTH:
                    y++;
                    break;
                case WEST:
                    x--;
                    break;
            }
        }
        System.out.println(infectionBursts);
    }

    private Direction turnLeft(Direction direction) {
        int index = (direction.ordinal() - 1 + 4) % 4;
        return Direction.values()[index];
    }

    private Direction turnRight(Direction direction) {
        int index = (direction.ordinal() + 1) % 4;
        return Direction.values()[index];
    }

    @Override
    protected void part2() {
        Map<Vector, Status> map = new HashMap<>();
        Direction direction = Direction.NORTH;
        for (Node n : infected2) {
            map.put(n.getPosition(), Status.INFECTED);
        }
        Vector current = new Vector(0, 0);
        int infectionBursts = 0;
        for (int i = 0; i < 10000000; i++) {
            Status currentStatus = map.get(current);
            if (currentStatus == null || currentStatus == Status.CLEAN) {
                direction = turnLeft(direction);
                map.put(current, Status.WEAKENED);
            } else if (currentStatus == Status.WEAKENED) {
                map.put(current, Status.INFECTED);
                infectionBursts++;
            } else if (currentStatus == Status.INFECTED) {
                direction = turnRight(direction);
                map.put(current, Status.FLAGGED);
            } else {
                direction = turnRight(direction);
                direction = turnRight(direction);
                map.put(current, Status.CLEAN);
            }
            switch (direction) {
                case NORTH:
                    current = current.add(new Vector(0, -1));
                    break;
                case EAST:
                    current = current.add(new Vector(1, 0));
                    break;
                case SOUTH:
                    current = current.add(new Vector(0, 1));
                    break;
                case WEST:
                    current = current.add(new Vector(-1, 0));
                    break;
            }
        }
        System.out.println(infectionBursts);
    }

    @Override
    protected void setup() {
        infected = new ArrayList<>();
        infected2 = new ArrayList<>();
        int origoX = lines.get(0).length() / 2;
        int origoY = lines.size() / 2;
        for (int i = 0; i < lines.size(); i++) {
            String row = lines.get(i);
            for (int j = 0; j < row.length(); j++) {
                if (row.charAt(j) == '#') {
                    infected.add(new Node(j - origoX, i - origoY));
                    infected2.add(new Node(j - origoX, i - origoY));
                }
            }
        }
    }
}
