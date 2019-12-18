package aoc.utils.output;

import aoc.utils.geometry.Point2D;

import java.util.Map;

public class OutputUtils {
    public static void printArr(char[] arr) {
        for (char c : arr) {
            System.out.print(c);
        }
    }

    public static void printMatrix(char[][] matrix) {
        for (char[] arr : matrix) {
            printArr(arr);
            System.out.println();
        }
    }

    public static void printMap(Map<Point2D, Character> map, int width, int height) {
        for (int row = 0; row < height; row++) {
            for (int pos = 0; pos < width; pos++) {
                char c = map.getOrDefault(new Point2D(pos, row), '.');
                System.out.print(c);
            }
            System.out.println();
        }
    }
}
