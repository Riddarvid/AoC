package aoc15.days.day5;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day5 extends Day {
    private List<Character> vowels;
    private List<String> illegal;

    @Override
    public long part1() {
        int nice = 0;
        for (String string : lines) {
            if (isNice1(string)) {
                nice++;
            }
        }
        return nice;
    }

    @Override
    public long part2() {
        int nice = 0;
        for (String string : lines) {
            if (isNice2(string)) {
                nice++;
            }
        }
        return nice;
    }

    @Override
    public void setup() {
        vowels = new ArrayList<>();
        vowels.add('a');
        vowels.add('e');
        vowels.add('i');
        vowels.add('o');
        vowels.add('u');
        illegal = new ArrayList<>();
        illegal.add("ab");
        illegal.add("cd");
        illegal.add("pq");
        illegal.add("xy");
    }

    private boolean isNice1(String string) {
        return hasThreeVowels(string) && hasDoubleLetter(string) && isNotIllegal(string);
    }

    private boolean isNotIllegal(String string) {
        for (String s : illegal) {
            if (string.contains(s)) {
                return false;
            }
        }
        return true;
    }

    private boolean hasDoubleLetter(String string) {
        char[] chars = string.toCharArray();
        for (int i = 0; i < string.length() - 1; i++) {
            if (chars[i] == chars[i + 1]) {
                return true;
            }
        }
        return false;
    }

    private boolean hasThreeVowels(String string) {
        int vowelCount = 0;
        for (char c : string.toCharArray()) {
            if (vowels.contains(c)) {
                vowelCount++;
            }
            if (vowelCount == 3) {
                return true;
            }
        }
        return false;
    }

    private boolean isNice2(String string) {
        return hasRepeatCouple(string) && hasRepeat(string);
    }

    private boolean hasRepeat(String string) {
        char[] chars = string.toCharArray();
        for (int i = 0; i < string.length() - 2; i++) {
            if (chars[i] == chars[i + 2]) {
                return true;
            }
        }
        return false;
    }

    private boolean hasRepeatCouple(String string) {
        for (int i = 0; i < string.length() - 3; i ++) {
            String subString = string.substring(i, i + 2);
            if (string.indexOf(subString) != string.lastIndexOf(subString) && string.indexOf(subString) + 1 != string.lastIndexOf(subString)) {
                return true;
            }
        }
        return false;
    }
}
