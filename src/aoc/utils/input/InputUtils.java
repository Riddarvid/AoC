package aoc.utils.input;

import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

public class InputUtils {

    public static List<String> getLines(File f) {
        try {
            return Files.readAllLines(f.toPath());
        } catch (FileNotFoundException e) {
            System.out.println("File not found");
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static List<Integer> getInts(String input) {
        int start = 0;
        List<Integer> ints = new ArrayList<>();
        while (start < input.length()) {
            while (start < input.length() && !Character.isDigit(input.charAt(start))) {
                start++;
            }
            if (start >= input.length()) {
                break;
            }
            int end = start + 1;
            while (end < input.length() && Character.isDigit(input.charAt(end))) {
                end ++;
            }
            ints.add(Integer.parseInt(input.substring(start, end)));
            start = end + 1;
        }
        return ints;
    }

    public static List<Long> getIntsNegative(String input) {
        int start = 0;
        List<Long> ints = new ArrayList<>();
        while (start < input.length()) {
            while (start < input.length() && !Character.isDigit(input.charAt(start)) && input.charAt(start) != '-') {
                start++;
            }
            if (start >= input.length()) {
                break;
            }
            int end = start + 1;
            while (end < input.length() && Character.isDigit(input.charAt(end))) {
                end ++;
            }
            long toBeAdded = Long.parseLong(input.substring(start, end));
            ints.add(toBeAdded);
            start = end + 1;
        }
        return ints;
    }

    public static List<String> getTokens(String input, char delimiter) {
        int start = 0;
        List<String> tokens = new ArrayList<>();
        while (start < input.length()) {
            while (start < input.length() && input.charAt(start) == delimiter) {
                start++;
            }
            if (start >= input.length()) {
                break;
            }
            int end = start + 1;
            while (end < input.length() && input.charAt(end) != delimiter) {
                end ++;
            }
            tokens.add(input.substring(start, end));
            start = end + 1;
        }
        return tokens;
    }

    public static List<Integer> getInts(char[] data, int start, int end) {
        char[] subData = new char[end - start];
        System.arraycopy(data, start, subData, 0, end - start);
        return getInts(new String(subData));
    }
}