package aoc.days.day6;

import java.util.Comparator;

public class StringComparator implements Comparator<Character> {
    private final String string;
    private final boolean inverted;

    public StringComparator(String string, boolean inverted) {
        this.string = string;
        this.inverted = inverted;
    }

    @Override
    public int compare(Character c1, Character c2) {
        if (inverted) {
            return count(c1) - count(c2);
        }
        return count(c2) - count(c1);
    }

    private int count(char target) {
        int sum = 0;
        for (char c : string.toCharArray()) {
            if (c == target) {
                sum++;
            }
        }
        return sum;
    }
}
