package aoc.days.day3;

import aoc.days.Day;

import java.util.InputMismatchException;

public class Day3 extends Day {
    private int input;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        int i = 1;
        while (i * i < input) {
            i += 2;
        }
        int x = (i) / 2;
        int y = -x;
        int offset = input - ((i - 2) * (i - 2));
        int section = offset / (i - 1);
        switch (section) {
            case 0:
                y += offset;
                break;
            case 1:
                y += (i - 1);
                offset -= (i - 1);
                x -= offset;
                break;
            case 2:
                y += (i - 1);
                offset -= (i - 1);
                x -= (i - 1);
                offset -= (i - 1);
                y -= offset;
                break;
            case 3:
                offset -= 3 * (i - 1);
                x += offset - (i - 1);
                break;
            default:
                throw new InputMismatchException();
        }
        System.out.println(getDistance(x, y));
    }

    private int getDistance(int x, int y) {
        return Math.abs(x) + Math.abs(y);
    }

    @Override
    protected void part2() {
        int[][] grid = new int[101][101];
        int x = 51;
        int y = 51;
        grid[x][y] = 1;
        Direction direction = Direction.RIGHT;
        while (grid[x][y] <= input) {
            switch (direction) {
                case UP:
                    y++;
                    if (grid[x - 1][y] == 0) {
                        direction = Direction.LEFT;
                    }
                    break;
                case LEFT:
                    x--;
                    if (grid[x][y - 1] == 0) {
                        direction = Direction.DOWN;
                    }
                    break;
                case DOWN:
                    y--;
                    if (grid[x + 1][y] == 0) {
                        direction = Direction.RIGHT;
                    }
                    break;
                case RIGHT:
                    x++;
                    if (grid[x][y + 1] == 0) {
                        direction = Direction.UP;
                    }
                    break;
            }
            grid[x][y] = getSum(grid, x, y);
        }
        System.out.println(grid[x][y]);
    }

    private int getSum(int[][] grid, int x, int y) {
        int sum = 0;
        for (int i = -1; i < 2; i++) {
            for (int j = -1; j < 2; j++) {
                sum += grid[x + i][y + j];
            }
        }
        return sum;
    }

    @Override
    protected void setup() {
        input = Integer.parseInt(lines.get(0));
    }
}
