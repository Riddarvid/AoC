package aoc.days.day18;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day18 extends Day {
    private List<Boolean> firstRow;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        System.out.println(numberOfSafe(generateRows(firstRow, 40)));
    }

    private int numberOfSafe(List<List<Boolean>> safeTiles) {
        int sum = 0;
        for (List<Boolean> row : safeTiles) {
            for (boolean tile : row) {
                if (tile) {
                    sum++;
                }
            }
        }
        return sum;
    }

    @Override
    protected void part2() {
        System.out.println(numberOfSafe(generateRows(firstRow, 400000)));
    }

    @Override
    protected void setup() {
        firstRow = new ArrayList<>();
        String s = lines.get(0);
        for (char c : s.toCharArray()) {
            firstRow.add(c == '.');
        }
    }

    private List<List<Boolean>> generateRows(List<Boolean> firstRow, int numberOfRows) {
        List<List<Boolean>> safeTiles = new ArrayList<>();
        safeTiles.add(firstRow);
        List<Boolean> lastRow = firstRow;
        for (int i = 1; i < numberOfRows; i++) {
            List<Boolean> nextRow = getNextRow(lastRow);
            safeTiles.add(nextRow);
            lastRow = nextRow;
        }
        return safeTiles;
    }

    private List<Boolean> getNextRow(List<Boolean> lastRow) {
        List<Boolean> nextRow = new ArrayList<>();
        nextRow.add(getNextTile(true, lastRow.get(0), lastRow.get(1)));
        for (int i = 0; i < lastRow.size() -2; i++) {
            nextRow.add(getNextTile(lastRow.get(i), lastRow.get(i + 1), lastRow.get(i + 2)));
        }
        nextRow.add(getNextTile(lastRow.get(lastRow.size() - 2), lastRow.get(lastRow.size() - 1), true));
        return nextRow;
    }

    private Boolean getNextTile(boolean left, boolean center, boolean right) {
        if ((!left && !center) && right) {
            return false;
        }
        if ((!center && !right) && left) {
            return false;
        }
        if ((center && right) && !left) {
            return false;
        }
        if ((left && center) && !right) {
            return false;
        }
        return true;
    }
}
