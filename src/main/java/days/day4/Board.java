package days.day4;

import aoc.math.geometry.Point;

import java.util.*;

public class Board {
    private final Square[][] grid;
    private final Map<Integer, Point> locationMap;
    private boolean hasBingo;
    private int sumOfUnmarked;

    public Board(List<List<Integer>> numbers) {
        grid = new Square[numbers.size()][numbers.get(0).size()];
        locationMap = new HashMap<>();
        hasBingo = false;
        for (int y = 0; y < numbers.size(); y++) {
            for (int x = 0; x < numbers.get(0).size(); x++) {
                int number = numbers.get(y).get(x);
                grid[y][x] = new Square(number);
                locationMap.put(number, new Point(x, y));
            }
        }
    }

    public boolean hasBingo() {
        return hasBingo;
    }

    public int getSumOfUnmarked() {
        return sumOfUnmarked;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Square[] row : grid) {
            for (Square square : row) {
                sb.append(square.toString()).append(' ');
            }
            sb.append('\n');
        }
        return sb.toString();
    }

    public void mark(int draw) {
        if (locationMap.containsKey(draw)) {
            Point location = locationMap.get(draw);
            grid[location.getY()][location.getX()].mark();
            checkIfBingo(location.getX(), location.getY());
        }
    }

    private void checkIfBingo(int x, int y) {
        checkRow(y);
        checkColumn(x);
    }

    private void checkRow(int row) {
        for (int column = 0; column < grid[0].length; column++) {
            if (!grid[row][column].isMarked()) {
                return;
            }
        }
        hasBingo = true;
        calculateSumOfUnmarked();
    }

    private void checkColumn(int column) {
        for (int row = 0; row < grid.length; row++) {
            if (!grid[row][column].isMarked()) {
                return;
            }
        }
        hasBingo = true;
        calculateSumOfUnmarked();
    }

    private void calculateSumOfUnmarked() {
        int sum = 0;
        for (Square[] row : grid) {
            for (Square square : row) {
                if (!square.isMarked()) {
                    sum += square.getNumber();
                }
            }
        }
        sumOfUnmarked = sum;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Board board = (Board) o;
        return hasBingo == board.hasBingo && sumOfUnmarked == board.sumOfUnmarked && Arrays.equals(grid, board.grid) && Objects.equals(locationMap, board.locationMap);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(locationMap, hasBingo, sumOfUnmarked);
        result = 31 * result + Arrays.hashCode(grid);
        return result;
    }

    public String getRowString(int row) {
        StringBuilder sb = new StringBuilder();
        for (int column = 0; column < 5; column++) {
            sb.append(grid[row][column].toString()).append(' ');
        }
        return sb.toString();
    }
}
