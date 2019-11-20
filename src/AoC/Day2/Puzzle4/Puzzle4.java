package AoC.Day2.Puzzle4;

import AoC.FileUtilities;

import java.io.File;

public class Puzzle4 {
    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Puzzle4/Input.txt"));
        for (int i = 0; i < input.length - 1; i++) {
            for (int j = i + 1; j < input.length; j++) {
                String stringA = input[i];
                String stringB = input[j];
                int different = 0;
                for (int pos = 0; pos < stringA.length(); pos++) {
                    if (stringA.charAt(pos) != stringB.charAt(pos)) {
                        different++;
                    }
                }
                if (different == 1) {
                    System.out.println(getCommon(stringA, stringB));
                }
            }
        }
    }

    public static String getCommon(String stringA, String stringB) {
        StringBuilder sb = new StringBuilder();
        for (int pos = 0; pos < stringA.length(); pos++) {
            if (stringA.charAt(pos) == stringB.charAt(pos)) {
                sb.append(stringA.charAt(pos));
            }
        }
        return sb.toString();
    }
}
