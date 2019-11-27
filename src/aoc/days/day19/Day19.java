package aoc.days.day19;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day19 extends Day {
    private char[][] grid;
    private Vector position;
    private int steps;

    public static void main(String[] args) {
        new Day19();
    }

    @Override
    protected void part1() {
        List<Character> passedLetters = new ArrayList<>();
        Vector direction = new Vector(0, 1);
        steps = 0;
        char current = grid[position.getY()][position.getX()];
        while (current != ' ') {
            steps++;
            if (Character.isAlphabetic(current)) {
                passedLetters.add(current);
            } else if (current == '+') {
                Vector newDirection = direction.flip();
                Vector newPosition = position.add(newDirection);
                if (grid[newPosition.getY()][newPosition.getX()] != ' ') {
                    direction = newDirection;
                } else {
                    direction = newDirection.invert();
                }
            }
            position = position.add(direction);
            current = grid[position.getY()][position.getX()];
        }
        printList(passedLetters);
    }

    private void printList(List<Character> passedLetters) {
        StringBuilder sb = new StringBuilder();
        for (char c : passedLetters) {
            sb.append(c);
        }
        System.out.println(sb);
    }

    @Override
    protected void part2() {
        System.out.println(steps);
    }

    @Override
    protected void setup() {
        grid = new char[lines.size()][lines.get(0).length()];
        for (int i = 0; i < lines.size(); i++) {
            String row = lines.get(i);
            for (int j = 0; j < row.length(); j++) {
                grid[i][j] = row.charAt(j);
            }
        }
        String firstRow = lines.get(0);
        for (int i = 0; i < firstRow.length(); i++) {
            if (firstRow.charAt(i) == '|') {
                position = new Vector(i, 0);
                break;
            }
        }
    }
}
