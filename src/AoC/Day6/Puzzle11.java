package AoC.Day6;

import AoC.FileUtilities;
import AoC.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Puzzle11 {


    public static void main(String[] args) {
        //char[] chars = {'A', 'B', 'C', 'D' }
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day6/Input.txt"));
        Point[] locs1 = new Point[input.length];
        for (int i = 0; i < locs1.length; i++) {
            locs1[i] = new Point(RegEx.getIntegers(input[i]).get(0), RegEx.getIntegers(input[i]).get(1));
        }
        Point[] locs2 = new Point[input.length];
        for (int i = 0; i < locs2.length; i++) {
            locs2[i] = new Point(RegEx.getIntegers(input[i]).get(0), RegEx.getIntegers(input[i]).get(1));
        }
        List<Point> points1 = new ArrayList<>();
        for (int x = -4000; x < 4000; x++) {
            for (int y = -4000; y < 4000; y++) {
                points1.add(new Point(x, y, locs1));
            }
        }
        List<Point> points2 = new ArrayList<>();
        for (int x = -5000; x < 5000; x++) {
            for (int y = -5000; y < 5000; y++) {
                points2.add(new Point(x, y, locs2));
            }
        }
        Arrays.sort(locs1);
        Arrays.sort(locs2);
        System.out.println(Arrays.toString(locs1));
        System.out.println(Arrays.toString(locs2));
        /*for (int row = 0; row < 2000; row++) {
            for (int col = 0; col < 2000; col++) {
                Point loc = points2.get(2000*row + col);
                if (loc.getOwner() == null) {
                    System.out.print('.');
                } else {

                }
            }
        }*/
    }
}
