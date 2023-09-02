package days.day23;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day23 extends Day {
    private Circle circle1;
    private Circle circle2;

    public static void main(String[] args) {
        new Day23().runAndPrint();
    }

    @Override
    public long part1() {
        for (int i = 0; i < 100; i++) {
            circle1.move();
        }
        List<Integer> labels = circle1.toList(1);
        StringBuilder sb = new StringBuilder();
        for (int label : labels) {
            sb.append(label);
        }
        return Integer.parseInt(sb.toString());
    }

    @Override
    public long part2() {
        for (int i = 0; i < 10_000_000; i++) {
            circle2.move();
        }
        long[] nextLabels = circle2.getNextLabels(1, 2);
        return nextLabels[0] * nextLabels[1];
    }

    @Override
    public void setup() {
        List<Integer> values = new ArrayList<>();
        for (char c : lines.get(0).toCharArray()) {
            values.add(Integer.parseInt(c + ""));
        }
        circle1 = new Circle(values);
        circle2 = new Circle(values, 1_000_000);
    }
}
