package aoc.days.day17;

import aoc.days.Day;

public class Day17 extends Day {
    private int stepDistance;

    public static void main(String[] args) {
        new Day17();
    }

    @Override
    protected void part1() {
        Buffer buffer = new Buffer(0);
        for (int i = 1; i <= 3; i++) {
            buffer.step(stepDistance);
            buffer.insert(i);
        }
        buffer.step(1);
        buffer.goToRoot();
        buffer.step(1);
        System.out.println(buffer.getCurrent());
    }

    @Override
    protected void part2() {
        int length = 2;
        int afterZero = 1;
        int current = 1;
        for (int i = 2; i <= 50_000_000; i++) {
            int nextCurrent = (current + stepDistance) % length + 1;
            length++;
            if (nextCurrent == 1) {
                afterZero = i;
            }
            current = nextCurrent;
        }
        System.out.println(afterZero);
    }

    private void print(Buffer buffer) {
        buffer.goToRoot();
        int root = buffer.getCurrent();
        System.out.print(root + " ");
        buffer.step(1);
        while (buffer.getCurrent() != root) {
            System.out.print(buffer.getCurrent() + " ");
            buffer.step(1);
        }
        System.out.println();
    }

    @Override
    protected void setup() {
        stepDistance = Integer.parseInt(lines.get(0));
    }
}
