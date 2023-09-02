package aoc.days.day5;

import aoc.days.Day;

public class Day5 extends Day {
    int[] offsets1;
    int[] offsets2;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        int steps = 0;
        int pointer = 0;
        while (pointer < offsets1.length && pointer >= 0) {
            steps++;
            int jump = offsets1[pointer];
            offsets1[pointer]++;
            pointer += jump;
        }
        System.out.println(steps);
    }

    @Override
    protected void part2() {
        int steps = 0;
        int pointer = 0;
        while (pointer < offsets2.length && pointer >= 0) {
            steps++;
            int jump = offsets2[pointer];
            if (jump >= 3) {
                offsets2[pointer]--;
            } else {
                offsets2[pointer]++;
            }
            pointer += jump;
        }
        System.out.println(steps);
    }

    @Override
    protected void setup() {
        offsets1 = new int[lines.size()];
        offsets2 = new int[lines.size()];
        for (int i = 0; i < lines.size(); i++) {
            offsets1[i] = Integer.parseInt(lines.get(i));
            offsets2[i] = Integer.parseInt(lines.get(i));
        }
    }
}
