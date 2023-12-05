package aoc15.days.day8;

import aoc.days.Day;

public class Day8 extends Day {

    private int numberOfLiterals;

    @Override
    public long part1() {
        numberOfLiterals = 0;
        int numberOfCharacters = 0;
        for (String string : lines) {
            numberOfLiterals += string.length();
            string = string.substring(1, string.length() - 1);
            int i = 0;
            char[] chars = string.toCharArray();
            while (i < chars.length) {
                if (chars[i] == '\\') {
                    if (chars[i + 1] == 'x') {
                        i += 4;
                    } else {
                        i += 2;
                    }
                } else {
                    i++;
                }
                numberOfCharacters++;
            }
        }
        return numberOfLiterals - numberOfCharacters;
    }

    @Override
    public long part2() {
        int numberOfEncoded = 0;
        for (String string : lines) {
            StringBuilder sb = new StringBuilder();
            sb.append('\"');
            char[] chars = string.toCharArray();
            for (int i = 0; i < string.length(); i++) {
                if (chars[i] == '\"') {
                    sb.append("\\\"");
                } else if (chars[i] == '\\') {
                    sb.append("\\\\");
                } else {
                    sb.append(chars[i]);
                }
            }
            sb.append('\"');
            numberOfEncoded += sb.length();
        }
        return numberOfEncoded - numberOfLiterals;
    }

    @Override
    public void setup() {

    }
}
