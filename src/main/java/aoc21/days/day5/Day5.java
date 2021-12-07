package aoc21.days.day5;

import aoc.days.Day;
import aoc.math.geometry.Point;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day5 extends Day {
    private List<Line> hydroLinesNoDiagonal;
    private List<Line> allHydroLines;

    public static void main(String[] args) {
        new Day5().runAndPrint();
    }

    public Day5() {
        super("input5");
    }

    private long getNumberOfIntersections(List<Line> hydroLines) {
        Set<Point> encounteredOnce = new HashSet<>();
        Set<Point> encounteredMultiple = new HashSet<>();
        for (Line line : hydroLines) {
            for (Point point : line.getPoints()) {
                if (encounteredMultiple.contains(point)) {
                    continue;
                }
                if (encounteredOnce.contains(point)) {
                    encounteredMultiple.add(point);
                    encounteredOnce.remove(point);
                    continue;
                }
                encounteredOnce.add(point);
            }
        }
        return encounteredMultiple.size();
    }

    @Override
    public long part1() {
        return getNumberOfIntersections(hydroLinesNoDiagonal);
    }

    @Override
    public long part2() {
        return getNumberOfIntersections(allHydroLines);
    }

    @Override
    public void setup() {
        hydroLinesNoDiagonal = new ArrayList<>();
        allHydroLines = new ArrayList<>();
        for (String inputString : lines) {
            List<Integer> values = ParsingUtils.getIntegers(inputString);
            int x1 = values.get(0);
            int y1 = values.get(1);
            int x2 = values.get(2);
            int y2 = values.get(3);
            allHydroLines.add(new Line(x1, y1, x2, y2));
            if (x1 == x2 || y1 == y2) {
                hydroLinesNoDiagonal.add(new Line(x1, y1, x2, y2));
            }
        }
    }
}
