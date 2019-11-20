package AoC.day18;

import AoC.Day;

public class Day18 extends Day {
    private boolean[][] originalGrid;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        boolean[][] grid = copyMatrix(originalGrid);
        //View view = new View(grid);
        for(int i = 0; i < 100; i++) {
            /*try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }*/
            grid = step(grid);
            //view.update(grid);
        }
        System.out.println(countLights(grid));
    }

    private int countLights(boolean[][] grid) {
        int sum = 0;
        for (int row = 0; row < 100; row++) {
            for (int col = 0; col < 100; col++) {
                if (grid[row][col]) {
                    sum++;
                }
            }
        }
        return sum;
    }

    private boolean[][] step(boolean[][] grid) {
        boolean[][] newGrid = new boolean[100][100];
        for (int row = 0; row < 100; row ++) {
            for (int col = 0; col < 100; col++) {
                int neighbours = calculateNeighbours(grid, row, col);
                boolean light = grid[row][col];
                if (light) {
                    if (neighbours != 2 && neighbours != 3) {
                        newGrid[row][col] = false;
                    } else {
                        newGrid[row][col] = true;
                    }
                } else {
                    if (neighbours == 3) {
                        newGrid[row][col] = true;
                    } else {
                        newGrid[row][col] = false;
                    }
                }
            }
        }
        return newGrid;
    }

    private boolean[][] step2(boolean[][] grid) {
        boolean[][] newGrid = new boolean[100][100];
        for (int row = 0; row < 100; row ++) {
            for (int col = 0; col < 100; col++) {
                if (row == 0 && col == 0) {
                    newGrid[row][col] = true;
                    continue;
                }
                if (row == 0 && col == 99) {
                    newGrid[row][col] = true;
                    continue;
                }
                if (row == 99 && col == 0) {
                    newGrid[row][col] = true;
                    continue;
                }
                if (row == 99 && col == 99) {
                    newGrid[row][col] = true;
                    continue;
                }
                int neighbours = calculateNeighbours(grid, row, col);
                boolean light = grid[row][col];
                if (light) {
                    if (neighbours != 2 && neighbours != 3) {
                        newGrid[row][col] = false;
                    } else {
                        newGrid[row][col] = true;
                    }
                } else {
                    if (neighbours == 3) {
                        newGrid[row][col] = true;
                    } else {
                        newGrid[row][col] = false;
                    }
                }
            }
        }
        return newGrid;
    }

    private int calculateNeighbours(boolean[][] grid, int row, int col) {
        int sum = 0;
        for (int i = -1; i <= 1; i++) {
            for (int j = -1; j <= 1; j++) {
                int y = row + i;
                int x = col + j;
                if (y >= 0 && x>= 0 && y < 100 && x < 100) {
                    if (grid[y][x]) {
                        sum++;
                    }
                }
            }
        }
        if (grid[row][col]) {
            sum--;
        }
        return sum;
    }

    @Override
    protected void part2() {
        boolean[][] grid = copyMatrix(originalGrid);
        grid[0][0] = true;
        grid[0][99] = true;
        grid[99][0] = true;
        grid[99][99] = true;
        //View view = new View(grid);
        for(int i = 0; i < 100; i++) {
            /*try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }*/
            grid = step2(grid);
            //view.update(grid);
        }
        System.out.println(countLights(grid));
    }

    @Override
    protected void setup() {
        originalGrid = new boolean[100][100];
        for (int row = 0; row < 100; row++) {
            String s = lines.get(row);
            for (int col = 0; col < 100; col++) {
                if (s.charAt(col) == '#') {
                    originalGrid[row][col] = true;
                }
            }
        }
    }

    boolean[][] copyMatrix(boolean[][] other) {
        boolean[][] copy = new boolean[other.length][other[0].length];
        for (int row = 0; row < other.length; row++) {
            System.arraycopy(other[row], 0, copy[row], 0, other[0].length);
        }
        return copy;
    }
}
