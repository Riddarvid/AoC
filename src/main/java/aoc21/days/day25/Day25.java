package aoc21.days.day25;

import aoc.days.Day;
import aoc.math.geometry.Point;

import java.util.HashSet;
import java.util.Set;

public class Day25 extends Day {
    private CucumberGrid cucumberGrid;

    @Override
    public void setup() {
        int height = lines.size();
        int width = lines.get(0).length();
        Set<Point> eastCucumbers = new HashSet<>();
        Set<Point> southCucumbers = new HashSet<>();
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                char c = lines.get(y).charAt(x);
                if (c == '>') {
                    eastCucumbers.add(new Point(x, y));
                } else if (c == 'v') {
                    southCucumbers.add(new Point(x, y));
                }
            }
        }
        CucumberGrid.width = width;
        CucumberGrid.height = height;
        cucumberGrid = new CucumberGrid(eastCucumbers, southCucumbers);
    }

    @Override
    public long part1() {
        CucumberGrid newGrid;
        int steps = 0;
        while (true) {
            steps++;
            newGrid = cucumberGrid.move();
            if (newGrid.equals(cucumberGrid)) {
                break;
            }
            cucumberGrid = newGrid;
        }
        return steps;
    }

    @Override
    public long part2() {
        return 0;
    }
}
