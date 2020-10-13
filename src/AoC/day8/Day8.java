package AoC.day8;

import riddarvid.aoc.days.Day;

public class Day8 extends Day {
    public static void main(String[] args) {
        new Day8();
    }

    private int numberOfLiterals;

    @Override
    protected void part1() {
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
        System.out.println(numberOfLiterals - numberOfCharacters);
    }

    @Override
    protected void part2() {
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
        System.out.println(numberOfEncoded - numberOfLiterals);
    }

    @Override
    protected void setup() {

    }
}
