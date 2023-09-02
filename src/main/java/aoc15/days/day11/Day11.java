package aoc15.days.day11;

import aoc.days.Day;

import java.util.HashMap;
import java.util.Map;

public class Day11 extends Day {
    
    private Map<Character, Integer> charToInt;
    private Map<Integer, Character> intToChar;
    private String oldPassword;
    private String password1;

    @Override
    public long part1() {
        int[] password = stringToArr(oldPassword);
        generateNextPassword(password);
        password1 = arrToString(password);
        System.out.println(arrToString(password));
        return 0;
    }

    private void generateNextPassword(int[] password) {
        while (true) {
            if (isLegal(password)) {
                if (hasDoublePair(password)) {
                    if (hasStraight(password)) {
                        break;
                    } else {
                        generateNextStraight(password);
                    }
                } else {
                    generateNextDoublePair(password);
                }
            } else {
                generateNextLegal(password);
            }
        }
    }

    private boolean isValid(int[] password) {
        return isLegal(password) && hasStraight(password) && hasDoublePair(password);
    }

    private void generateNextLegal(int[] password) {
        for (int i = 0; i < password.length; i++) {
            int c = password[i];
            if (c == 8 || c == 11 || c == 14) {
                inc(password, i);
                return;
            }
        }
    }

    private void generateNextDoublePair(int[] password) {
        if (hasPair(password, 0, password.length - 2)) {
            if (password[password.length - 1] <= password[password.length - 2]) {
                password[password.length - 1] = password[password.length - 2];
            } else {
                inc(password, password.length - 2);
            }
        } else {
            generateNextPair(password, password.length - 2);
        }
    }

    private boolean isLegal(int[] password) {
        for (int c : password) {
            if (c == 8 || c == 11 || c == 14) {
                return false;
            }
        }
        return true;
    }

    private void generateNextPair(int[] password, int end) {
        if (hasPair(password, 0, end)) {
            return;
        }
        if (password[end - 1] <= password[end - 2]) {
            password[end - 1] = password[end - 2];
            for (int i = end; i < password.length; i++) {
                password[i] = 0;
            }
        } else {
            inc(password, end - 2);
        }
    }

    private boolean hasPair(int[] password, int start, int end) {
        for (int i = start; i < end - 1; i++) {
            if (password[i] == password[i + 1]) {
                return true;
            }
        }
        return false;
    }

    private void generateNextStraight(int[] password) {
        if (hasStraight(password)) {
            return;
        }
        inc(password);
    }

    private boolean hasDoublePair(int[] password) {
        int i = 0;
        while (i < password.length - 3) {
            if (password[i] != password[i + 1]) {
                i++;
            } else {
                int j = i + 2;
                while (j < password.length - 1) {
                    if (password[j] == password[j + 1]) {
                        return true;
                    }
                    j++;
                }
                break;
            }
        }
        return false;
    }

    private boolean hasStraight(int[] password) {
        for (int i = 0; i < password.length - 2; i++) {
            if (password[i] + 1 == password[i + 1] && password[i + 1] + 1 == password[i +2]) {
                return true;
            }
        }
        return false;
    }

    private void inc(int[] password) {
        inc(password, password.length - 1);
    }

    private void inc(int[] password, int index) {
        for (int i = index; i >= 0; i--) {
            int val = password[i] + 1;
            if (val == 26) {
                password[i] = 0;
            } else {
                password[i] = val;
                break;
            }
        }
        for (int i = index + 1; i < password.length; i++) {
            password[i] = 0;
        }
    }

    private String arrToString(int[] arr) {
        StringBuilder sb = new StringBuilder();
        for (int i : arr) {
            sb.append(intToChar.get(i));
        }
        return sb.toString();
    }

    private int[] stringToArr(String string) {
        int[] arr = new int[8];
        for (int i = 0; i < 8; i++) {
            arr[i] = charToInt.get(string.charAt(i));
        }
        return arr;
    }

    @Override
    public long part2() {
        int[] password = stringToArr(password1);
        inc(password);
        generateNextPassword(password);
        System.out.println(arrToString(password));
        return 0;
    }

    @Override
    public void setup() {
        charToInt = new HashMap<>();
        intToChar = new HashMap<>();
        for (char c = 'a'; c <= 'z'; c++) {
            charToInt.put(c, c - 'a');
            intToChar.put(c - 'a', c);
        }
        oldPassword = lines.get(0);
    }
}
