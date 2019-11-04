package aoc.days.day14;

import aoc.days.Day;
import aoc.utils.KnotHash;

public class Day14 extends Day {
    boolean[][] disk;

    public static void main(String[] args) {
        new Day14();
    }

    @Override
    protected void part1() {
        System.out.println(used(disk));
    }

    private static int used(boolean[][] disk) {
        int sum = 0;
        for (int i = 0; i < disk.length; i++) {
            for (int j = 0; j < disk[0].length; j++) {
                if (disk[i][j]) {
                    sum++;
                }
            }
        }
        return sum;
    }

    @Override
    protected void part2() {
        int numberOfRegions = 0;
        int[][] regions = new int[128][128];
        for (int i = 0; i < disk.length; i++) {
            for (int j = 0; j < disk[0].length; j++) {
                if (disk[i][j]) {
                    if (regions[i][j] == 0) {
                        numberOfRegions++;
                        markNeighbours(regions, i, j, numberOfRegions);
                    }
                }
            }
        }
        System.out.println(numberOfRegions);
    }

    private void markNeighbours(int[][] regions, int i, int j, int numberOfRegions) {
        if (disk[i][j] && regions[i][j] == 0) {
            regions[i][j] = numberOfRegions;
            if (i + 1 < regions.length) {
                markNeighbours(regions, i + 1, j, numberOfRegions);
            }
            if (i - 1 >= 0) {
                markNeighbours(regions, i - 1, j, numberOfRegions);
            }
            if (j + 1 < regions[0].length) {
                markNeighbours(regions, i, j + 1, numberOfRegions);
            }
            if (j - 1 >= 0) {
                markNeighbours(regions, i, j - 1, numberOfRegions);
            }
        }
    }

    @Override
    protected void setup() {
        disk = new boolean[128][128];
        String baseKey = lines.get(0);
        for (int i = 0; i < 128; i++) {
            String key = baseKey + "-" + i;
            String row = KnotHash.hash(key);
            for (int j = 0; j < row.length(); j++) {
                int digit = Integer.parseInt(row.substring(j, j + 1), 16);
                for (int pos = 3; pos >= 0; pos--) {
                    disk[i][j * 4 + pos] = digit % 2 == 1;
                    digit /= 2;
                }
            }
        }
    }
}
