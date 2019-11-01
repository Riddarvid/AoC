package aoc.days.day2;

import aoc.FileUtilities;
import aoc.days.Day;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day2 extends Day {
    private List<List<Integer>> rows;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        int checksum = 0;
        for (List<Integer> row : rows) {
            int largest = row.get(0);
            int smallest = row.get(0);
            for (int n : row) {
                if (n > largest) {
                    largest = n;
                }
                if (n < smallest) {
                    smallest = n;
                }
            }
            checksum += largest - smallest;
        }
        System.out.println(checksum);
    }

    @Override
    protected void part2() {
        int sum = 0;
        for (List<Integer> row : rows) {
            sum += getOnlyDivisible(row);
        }
        System.out.println(sum);
    }

    private int getOnlyDivisible(List<Integer> row) {
        for (int i = 0; i < row.size() - 1; i++) {
            for (int j = i + 1; j < row.size(); j++) {
                int larger;
                int smaller;
                if (row.get(i) > row.get(j)) {
                    larger = row.get(i);
                    smaller = row.get(j);
                } else {
                    larger = row.get(j);
                    smaller = row.get(i);
                }
                if (larger % smaller == 0) {
                    return larger/smaller;
                }
            }
        }
        throw new InputMismatchException();
    }

    @Override
    protected void setup() {
        rows = new ArrayList<>();
        for (String string : lines) {
            List<String> tokens = FileUtilities.getTokens(string, '\t');
            List<Integer> row = new ArrayList<>();
            for (String token : tokens) {
                row.add(Integer.parseInt(token));
            }
            rows.add(row);
        }
    }
}
