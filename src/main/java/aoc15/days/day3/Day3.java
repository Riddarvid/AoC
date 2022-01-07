package aoc15.days.day3;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    private char[] instructions;

    @Override
    public long part1() {
        Actor santa = new Actor();
        List<House> houses = new ArrayList<>();
        houses.add(new House(0, 0, 1));
        for (char c : instructions) {
            switch (c) {
                case '^' -> santa.up();
                case 'v' -> santa.down();
                case '>' -> santa.right();
                case '<' -> santa.left();
                default -> System.out.println("Unsupported operator");
            }
            House house = new House(santa.getX(), santa.getY());
            if (houses.contains(house)) {
                houses.get(houses.indexOf(house)).addPresent();
            } else {
                houses.add(house);
            }
        }
        return houses.size();
    }

    @Override
    public long part2() {
        Actor santa = new Actor();
        Actor robot = new Actor();
        Actor current = santa;
        List<House> houses = new ArrayList<>();
        houses.add(new House(0, 0, 1));
        for (char c : instructions) {
            switch (c) {
                case '^' -> current.up();
                case 'v' -> current.down();
                case '>' -> current.right();
                case '<' -> current.left();
                default -> System.out.println("Unsupported operator");
            }
            House house = new House(current.getX(), current.getY());
            if (houses.contains(house)) {
                houses.get(houses.indexOf(house)).addPresent();
            } else {
                houses.add(house);
            }
            if (current == santa) {
                current = robot;
            } else {
                current = santa;
            }
        }
        return houses.size();
    }

    @Override
    public void setup() {
        instructions = lines.get(0).toCharArray();
    }
}
