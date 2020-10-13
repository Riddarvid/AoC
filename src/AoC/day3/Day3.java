package AoC.day3;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    private char[] instructions;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        Actor santa = new Actor();
        List<House> houses = new ArrayList<>();
        houses.add(new House(0, 0, 1));
        for (char c : instructions) {
            switch (c) {
                case '^':
                    santa.up();
                    break;
                case 'v':
                    santa.down();
                    break;
                case '>':
                    santa.right();
                    break;
                case '<':
                    santa.left();
                    break;
                default:
                    System.out.println("Unsupported operator");
            }
            House house = new House(santa.getX(), santa.getY());
            if (houses.contains(house)) {
                houses.get(houses.indexOf(house)).addPresent();
            } else {
                houses.add(house);
            }
        }
        System.out.println(houses.size());
    }

    @Override
    protected void part2() {
        Actor santa = new Actor();
        Actor robot = new Actor();
        Actor current = santa;
        List<House> houses = new ArrayList<>();
        houses.add(new House(0, 0, 1));
        for (char c : instructions) {
            switch (c) {
                case '^':
                    current.up();
                    break;
                case 'v':
                    current.down();
                    break;
                case '>':
                    current.right();
                    break;
                case '<':
                    current.left();
                    break;
                default:
                    System.out.println("Unsupported operator");
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
        System.out.println(houses.size());
    }

    @Override
    protected void setup() {
        instructions = lines.get(0).toCharArray();
    }
}
