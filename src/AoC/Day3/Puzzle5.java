package AoC.Day3;

import AoC.FileUtilities;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Puzzle5 {
    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day3/Input.txt"));
        int[][] fabric = new int[1000][1000];
        List<Claim> claims = new ArrayList<>();
        for (String s : input) {
            Claim claim = getClaim(s);
            claims.add(claim);
            for (int row = claim.getOffsetY(); row < claim.getOffsetY() + claim.getHeight(); row++) {
                for (int col = claim.getOffsetX(); col < claim.getOffsetX() + claim.getWidth(); col++) {
                    fabric[row][col] = fabric[row][col] + 1;
                }
            }
        }
        for (Claim c : claims) {
            if (!doesOverlap(fabric, c)) {
                System.out.println(c.getId());
            }
        }
    }

    private static boolean doesOverlap(int[][] matrix, Claim claim) {
        for (int row = claim.getOffsetY(); row < claim.getOffsetY() + claim.getHeight(); row++) {
            for (int col = claim.getOffsetX(); col < claim.getOffsetX() + claim.getWidth(); col++) {
                if (matrix[row][col] != 1) {
                    return true;
                }
            }
        }
        return false;
    }

    private static int getNumberOfOverlaps(int[][] matrix) {
        int sum = 0;
        for (int row = 0; row < matrix.length; row++) {
            for (int col = 0; col < matrix[row].length; col++) {
                if (matrix[row][col] > 1) {
                    sum++;
                }
            }
        }
        return sum;
    }

    private static Claim getClaim(String s) {
        int id;
        int offsetX;
        int offsetY;
        int width;
        int height;

        int pos = 1;
        int start = 1;
        while (Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        id = Integer.parseInt(s.substring(start, pos));
        while (!Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        start = pos;
        while (Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        offsetX = Integer.parseInt(s.substring(start, pos));
        pos++;
        start = pos;
        while (Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        offsetY = Integer.parseInt(s.substring(start, pos));
        pos += 2;
        start = pos;
        while (Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        width = Integer.parseInt(s.substring(start, pos));
        pos++;
        start = pos;
        while (pos < s.length() && Character.isDigit(s.charAt(pos))) {
            pos++;
        }
        height = Integer.parseInt(s.substring(start, pos));
        return new Claim(id, offsetX, offsetY, width, height);
    }
}
