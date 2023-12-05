package aoc.days.day3;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    private List<Triangle> triangles;
    private List<Triangle> triangles2;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (Triangle triangle : triangles) {
            if (triangle.isValid()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        int sum = 0;
        for (Triangle triangle : triangles2) {
            if (triangle.isValid()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void setup() {
        triangles = new ArrayList<>();
        for (String s : lines) {
            List<Integer> values = InputUtils.getInts(s);
            Triangle triangle = new Triangle(values.get(0), values.get(1), values.get(2));
            triangles.add(triangle);
        }
        triangles2 = new ArrayList<>();
        for (int column = 0; column < 3; column++) {
            for (int i = 0; i < lines.size(); i += 3) {
                triangles2.add(parseTriangle(column, i));
            }
        }
    }

    private Triangle parseTriangle(int column, int i) {
        int side1 = InputUtils.getInts(lines.get(i)).get(column);
        int side2 = InputUtils.getInts(lines.get(i + 1)).get(column);
        int side3 = InputUtils.getInts(lines.get(i + 2)).get(column);
        return new Triangle(side1, side2, side3);
    }
}
