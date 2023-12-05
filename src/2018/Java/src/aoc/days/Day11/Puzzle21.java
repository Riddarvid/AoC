package aoc.days.Day11;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Puzzle21 {
    public static void main(String[] args) {
        new Puzzle21();
    }

    public Puzzle21() {
        int[][] powerLevels = initPowerLevels(5535, 300);
        List<Entry> entries = calculateMaxPerSize(powerLevels);
        Entry max = getMax(entries);
        System.out.println(max.toString());
    }

    public static int[][] initPowerLevels(int serialNumber, int size) {
        int[][] powerLevels = new int[size][size];
        for (int x = 0; x < size; x++) {
            for (int y = 0; y < size; y++) {
                int rackID = x + 11;
                int power = rackID * (y + 1);
                power += serialNumber;
                power *= rackID;
                power /= 100;
                power %= 10;
                power -= 5;
                powerLevels[x][y] = power;
            }
        }
        return powerLevels;
    }

    public  static int[][] calculateValues(int[][] powerLevels, int size) {
        int numberOfEntries = powerLevels.length - size + 1;
        int[][] values = new int[numberOfEntries][numberOfEntries];
        for (int x = 0; x < numberOfEntries; x++) {
            for (int y = 0; y < numberOfEntries; y++) {
                values[x][y] = calculateValue(powerLevels, size, x, y);
            }
        }
        return values;
    }

    public static int calculateValue(int[][] powerLevels, int size, int x, int y) {
        int maxX = x + size;
        int maxY = y + size;
        int sum = 0;
        for (int i = x; i < maxX; i++) {
            for (int j = y; j < maxY; j++) {
                sum += powerLevels[i][j];
            }
        }
        return sum;
    }

    public static Entry getMaxEntry(int[][] values, int size) {
        Entry max = new Entry(1, 1, size, values[0][0]);
        for (int x = 0; x < values.length; x++) {
            for (int y = 0; y < values.length; y++) {
                if (values[x][y] > max.getValue()) {
                    max = new Entry(x + 1, y + 1, size, values[x][y]);
                }
            }
        }
        return max;
    }

    public static List<Entry> calculateMaxPerSize(int[][] powerLevels) {
        List<Entry> entries = new ArrayList<>();
        for (int i = 0; i < powerLevels.length; i++) {
            int[][] values = calculateValues(powerLevels, i + 1);
            entries.add(getMaxEntry(values, i + 1));
        }
        return entries;
    }

    public static Entry getMax(List<Entry> entries) {
        Entry max = entries.get(0);
        for (Entry e : entries) {
            if (e.getValue() > max.getValue()) {
                max = e;
            }
        }
        return max;
    }

    public static void printMatrix(int[][] matrix) {
        for (int[] row : matrix) {
            System.out.println(Arrays.toString(row));
        }
    }
}
