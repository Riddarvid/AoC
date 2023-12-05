package days.day6;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day6 extends Day {
    private List<Group> groups;

    public static void main(String[] args) {
        new Day6();
    }

    @Override
    public long part1() {
        int sum = 0;
        for (Group group : groups) {
            sum += group.getNumberOfAnswers();
        }
        return sum;
    }

    @Override
    public long part2() {
        int sum = 0;
        for (Group group : groups) {
            sum += group.getNumberEveryone();
        }
        return sum;
    }

    @Override
    public void setup() {
        groups = new ArrayList<>();
        Group current = new Group();
        for (String s : lines) {
            if (s.equals("")) {
                groups.add(current);
                current = new Group();
            } else {
                current.addAnswer(s);
            }
        }
        groups.add(current);
    }
}
