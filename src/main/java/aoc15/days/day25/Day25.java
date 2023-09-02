package aoc15.days.day25;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.List;

public class Day25 extends Day {
    private int row;
    private int column;

    @Override
    public long part1() {
        int index = getIndex(row, column);
        return getCode(index);
    }

    private void printCodes(int maxRow, int maxCol) {
        for (int row = 1; row <= maxRow; row++) {
            for (int col = 1; col <= maxCol; col++) {
                int index = getIndex(row, col);
                System.out.print(getCode(index) + "\t");
            }
            System.out.println();
        }
    }

    private long getCode(int index) {
        long code = 20151125;
        for (int i = 1; i < index; i++) {
            code = code * 252533;
            code = code % 33554393;
        }
        return code;
    }

    private int getIndex(int row, int column) {
        int side = row + column - 2;
        int base = side * (side + 1) / 2;
        return base + column;
    }

    @Override
    public long part2() {
        return 0;
    }

    @Override
    public void setup() {
        List<Integer> values = ParsingUtils.getIntegers(lines.get(0));
        row = values.get(0);
        column = values.get(1);
    }
}
