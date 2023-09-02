package aoc.days.day4;

import java.util.Comparator;

public class RoomComparator implements Comparator<Character> {
    private final String name;

    public RoomComparator(String name) {
        this.name = name;
    }

    @Override
    public int compare(Character c1, Character c2) {
        int diff = count(c2) - count(c1);
        if (diff == 0) {
            return c1 - c2;
        } else {
            return diff;
        }
    }

    private int count(char target) {
        int sum = 0;
        for (char c : name.toCharArray()) {
            if (c == target) {
                sum++;
            }
        }
        return sum;
    }
}
