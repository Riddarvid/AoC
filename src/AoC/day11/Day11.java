package AoC.day11;

import AoC.Day;

import java.util.HashMap;
import java.util.Map;

public class Day11 extends Day {
    public static void main(String[] args) {
        new Day11();
    }
    
    private Map<Character, Integer> charToInt;
    private Map<Integer, Character> intToChar;
    private String oldPassword;
    private String password1;

    @Override
    protected void part1() {
        int[] password = stringToArr(oldPassword);
        generateNextPassword(password);
        password1 = arrToString(password);
        System.out.println(arrToString(password));
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
        for (int i = 0; i < password.length; i++) {
            int c = password[i];
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
    protected void part2() {
        int[] password = stringToArr(password1);
        inc(password);
        generateNextPassword(password);
        System.out.println(arrToString(password));
    }

    @Override
    protected void setup() {
        charToInt = new HashMap<>();
        intToChar = new HashMap<>();
        charToInt.put('a', 0);
        charToInt.put('b', 1);
        charToInt.put('c', 2);
        charToInt.put('d', 3);
        charToInt.put('e', 4);
        charToInt.put('f', 5);
        charToInt.put('g', 6);
        charToInt.put('h', 7);
        charToInt.put('i', 8);
        charToInt.put('j', 9);
        charToInt.put('k', 10);
        charToInt.put('l', 11);
        charToInt.put('m', 12);
        charToInt.put('n', 13);
        charToInt.put('o', 14);
        charToInt.put('p', 15);
        charToInt.put('q', 16);
        charToInt.put('r', 17);
        charToInt.put('s', 18);
        charToInt.put('t', 19);
        charToInt.put('u', 20);
        charToInt.put('v', 21);
        charToInt.put('w', 22);
        charToInt.put('x', 23);
        charToInt.put('y', 24);
        charToInt.put('z', 25);
        intToChar.put(0, 'a');
        intToChar.put(1, 'b');
        intToChar.put(2, 'c');
        intToChar.put(3, 'd');
        intToChar.put(4, 'e');
        intToChar.put(5, 'f');
        intToChar.put(6, 'g');
        intToChar.put(7, 'h');
        intToChar.put(8, 'i');
        intToChar.put(9, 'j');
        intToChar.put(10, 'k');
        intToChar.put(11, 'l');
        intToChar.put(12, 'm');
        intToChar.put(13, 'n');
        intToChar.put(14, 'o');
        intToChar.put(15, 'p');
        intToChar.put(16, 'q');
        intToChar.put(17, 'r');
        intToChar.put(18, 's');
        intToChar.put(19, 't');
        intToChar.put(20, 'u');
        intToChar.put(21, 'v');
        intToChar.put(22, 'w');
        intToChar.put(23, 'x');
        intToChar.put(24, 'y');
        intToChar.put(25, 'z');
        oldPassword = lines.get(0);
    }
}
