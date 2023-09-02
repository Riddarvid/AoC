package aoc.utils.input;

import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    public static List<Long> getLongs(String input) {
        Pattern p = Pattern.compile("[0-9]+");
        Matcher m = p.matcher(input);
        List<Long> longs = new ArrayList<>();
        while (m.find()) {
            longs.add(Long.parseLong(m.group()));
        }
        return longs;
    }

    public static List<Integer> getInts(String input) {
        Pattern p = Pattern.compile("[0-9]+");
        Matcher m = p.matcher(input);
        List<Integer> ints = new ArrayList<>();
        while (m.find()) {
            ints.add(Integer.parseInt(m.group()));
        }
        return ints;
    }

    public static List<Integer> getIntsNegative(String input) {
        Pattern p = Pattern.compile("-?[0-9]+");
        Matcher m = p.matcher(input);
        List<Integer> ints = new ArrayList<>();
        while (m.find()) {
            ints.add(Integer.parseInt(m.group()));
        }
        return ints;
    }

    public static List<Long> getLongsNegative(String input) {
        Pattern p = Pattern.compile("-?[0-9]+");
        Matcher m = p.matcher(input);
        List<Long> longs = new ArrayList<>();
        while (m.find()) {
            longs.add(Long.parseLong(m.group()));
        }
        return longs;
    }

    public static List<String> getTokens(String input, String delimiter) {
        String[] tokens = input.split(delimiter);
        return Arrays.asList(tokens);
    }

    public static List<Integer> getInts(char[] data, int start, int end) {
        char[] subData = new char[end - start];
        System.arraycopy(data, start, subData, 0, end - start);
        return getInts(new String(subData));
    }
}