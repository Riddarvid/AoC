package AoC.day25;

import AoC.Day;
import AoC.FileUtilities;

import java.util.List;

public class Day25 extends Day {
    private int row;
    private int column;

    public static void main(String[] args) {
        new Day25();
    }

    @Override
    protected void part1() {
        int index = getIndex(row, column);
        System.out.println(getCode(index));
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
    protected void part2() {

    }

    @Override
    protected void setup() {
        List<Integer> values = FileUtilities.getInts(lines.get(0));
        row = values.get(0);
        column = values.get(1);
    }
}
