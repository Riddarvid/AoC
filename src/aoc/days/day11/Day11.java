package aoc.days.day11;

import riddarvid.aoc.days.Day;
import aoc.utils.geometry.Point2D;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static aoc.utils.output.Constants.*;

public class Day11 extends Day {
    private long[] memory;

    public static void main(String[] args) {
        new Day11();
    }

    @Override
    protected void part1() {
        PaintRobot pr = new PaintRobot(memory);
        pr.run();
        System.out.println(pr.getNPainted());
    }

    @Override
    protected void part2() {
        PaintRobot pr = new PaintRobot(memory);
        pr.set(0, 0, true);
        pr.run();
        printGrid(pr.getPainted());
    }

    private void printGrid(Map<Point2D, Boolean> painted) {
        List<Point2D> white = new ArrayList<>();
        for (Point2D p : painted.keySet()) {
            if (painted.get(p)) {
                white.add(p);
            }
        }
        for (int row = -10; row < 10; row++) {
            for (int pos = 0; pos < 80; pos++) {
                if (white.contains(new Point2D(pos, row))) {
                    System.out.print(ANSI_RED + BLOCK + ANSI_RESET);
                } else {
                    System.out.print(ANSI_BLACK + BLOCK + ANSI_RESET);
                }
            }
            System.out.println();
        }
    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}
