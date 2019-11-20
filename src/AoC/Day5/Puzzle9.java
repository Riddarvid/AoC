package AoC.Day5;

import AoC.FileUtilities;

import java.io.File;

public class Puzzle9 {
    public static void main(String[] args) {
        String input = FileUtilities.getArrayFromFile(new File("src/AoC/Day5/Dag5.txt"))[0];
        /*int shortest = 1000000000;
        for (char c = 'A'; c <= 'Z'; c++) {
            String collapsed = collapse(reduce(input, c));
            if (collapsed.length() < shortest) {
                shortest = collapsed.length();
            }
        }*/
        //System.out.println(shortest);
        long start = System.nanoTime();
        String output = collapse(input);
        //System.out.println("Iterations:" + output);
        System.out.println("Time:" + (System.nanoTime() - start));
        //System.out.println(output.length());
    }

    public static String reduce(String input, char c) {
        StringBuilder sb = new StringBuilder(input);
        int i = 0;
        while (i < sb.length()) {
            if ((sb.charAt(i) == c) || (sb.charAt(i) == c + 32)) {
                sb.deleteCharAt(i);
            } else {
                i++;
            }
        }
        return sb.toString();
    }

    public static boolean shouldReact(char a, char b) {
        return (Math.abs((int)a - (int)b) == 32);
    }

    public static String collapse(String input) {
        StringBuilder sb = new StringBuilder(input);
        int j = 0;
        while (j < sb.length() - 1) {
            if (shouldReact(sb.charAt(j), sb.charAt(j + 1))) {
                sb.delete(j, j + 2);
                j--;
                if (j < 0) {
                    j = 0;
                }
            } else {
                j++;
            }
        }
        /*for (int i = 0; i < sb.length() - 1; i++) {
            if (shouldReact(sb.charAt(i), sb.charAt(i + 1))) {
                sb.delete(i, i+2);
                i = -1;
            }
        }*/
        return sb.toString();
    }

    public static int collapseAlt(String input) {
        boolean hasReduced;
        StringBuilder sb = new StringBuilder(input);
        int iterations = 0;
        do {
            iterations++;
            hasReduced = false;
            for (int i = 0; i < sb.length() - 1; i++) {
                if (shouldReact(sb.charAt(i), sb.charAt(i + 1))) {
                    sb.delete(i, i + 2);
                    i--;
                    hasReduced = true;
                }
            }
        } while (hasReduced);
        return iterations;
    }
}
