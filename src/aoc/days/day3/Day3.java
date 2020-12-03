package aoc.days.day03;

import riddarvid.aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.geometry.Point2D;

import java.util.InputMismatchException;
import java.util.List;
import java.util.PriorityQueue;

public class Day3 extends Day {
    private Line l1;
    private Line l2;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        PriorityQueue<Intersection> intersections = new PriorityQueue<>(new IntersectionComparator());
        intersections.addAll(l1.getIntersections(l2));
        System.out.println(intersections.peek().getPoint().distanceFrom(new Point2D(0, 0)));
    }

    @Override
    protected void part2() {
        PriorityQueue<Intersection> intersections = new PriorityQueue<>(new StepComparator());
        intersections.addAll(l1.getIntersections(l2));
        System.out.println(intersections.peek().getSteps());
    }

    @Override
    protected void setup() {
        l1 = createLine(InputUtils.getTokens(lines.get(0), ','));
        l2 = createLine(InputUtils.getTokens(lines.get(1), ','));
    }

    private Line createLine(List<String> tokens) {
        Line line = new Line();
        int x = 0;
        int y = 0;
        for (String seg : tokens) {
            int length = Integer.parseInt(seg.substring(1));
            switch (seg.charAt(0)) {
                case 'R':
                    line.addSegment(new Segment(new Point2D(x + 1, y), new Point2D(x + length, y)));
                    x += length;
                    break;
                case 'D':
                    line.addSegment(new Segment(new Point2D(x, y - 1), new Point2D(x, y - length)));
                    y -= length;
                    break;
                case 'L':
                    line.addSegment(new Segment(new Point2D(x - 1, y), new Point2D(x - length, y)));
                    x -= length;
                    break;
                case 'U':
                    line.addSegment(new Segment(new Point2D(x, y + 1), new Point2D(x, y + length)));
                    y += length;
                    break;
                default:
                    throw new InputMismatchException(seg);
            }
        }
        return line;
    }
}
