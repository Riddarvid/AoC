package AoC;

import AoC.Day24.DamageType;

import java.util.*;

public abstract class RegEx {

    public static char[][] getCharArr(String[] input) {
        char[][] map = new char[input.length][];
        for (int i = 0; i < input.length; i++) {
            map[i] = input[i].toCharArray();
        }
        return map;
    }

    public static boolean[] getRules(String[] stringRules) {
        boolean[] rules = new boolean[32];
        for (int i = 2; i < stringRules.length; i++) {
            rules[binToDec(stringRules[i], 0, 5)] = stringRules[i].charAt(stringRules[i].length() - 1) == '#';
        }
        return rules;
    }

    public static boolean[] getInitialState(String[] input) {
        String start = input[0];
        int i = 0;
        while (start.charAt(i) != '#' && start.charAt(i) != '.') {
            i++;
        }
        start = start.substring(i);
        boolean[] states = new boolean[start.length()];
        for (int j = 0; j <start.length(); j++) {
            states[j] = start.charAt(j) == '#';
        }
        return states;
    }

    private static int binToDec(String stringRule, int start, int end) {
        int sum = 0;
        for (int i = start; i < end; i++) {
            sum *= 2;
            sum += stringRule.charAt(i) == '#' ? 1 : 0;
        }
        return sum;
    }

    public static Integer indexOf(Character c, String s) {
        int pos = 0;
        while (pos < s.length() && c != s.charAt(pos)) {
            pos++;
        }
        if (pos > s.length()){
            throw new NoSuchElementException();
        }
        return pos;
    }

    public static Action getAction(String string) {
        int pos = indexOf(']', string);
        pos += 2;
        switch (string.charAt(pos)) {
            case 'G':
                return Action.START;
            case 'f':
                return Action.SLEEP;
            case 'w':
                return Action.WAKE;
        }
        throw new InputMismatchException();
    }

    public static List<Integer> getIntegers(int start, int end, String string) {
        List<Integer> ints = new ArrayList<>();
        int subEnd;
        while (start < end) {
            while (start < end && !Character.isDigit(string.charAt(start)) && !(string.charAt(start) == '-')) {
                start++;
            }
            if (start == end) {
                break;
            }
            subEnd = start + 1;
            while (subEnd < end && Character.isDigit(string.charAt(subEnd))) {
                subEnd++;
            }
            ints.add(Integer.parseInt(string.substring(start, subEnd)));
            start = subEnd + 1;
        }
        return ints;
    }

    public static List<Integer> getIntegers(int start, char end, String string) {
        int pos = start;
        while (string.charAt(pos) != end) {
            pos++;
        }
        return getIntegers(start, pos, string);
    }

    public static List<Integer> getIntegers(char start, char end, String string) {
        int pos = 0;
        int numStart = 0;
        int numEnd;
        while (string.charAt(pos) != start) {
            numStart++;
        }
        return getIntegers(numStart, end, string);
    }

    public static List<Integer> getIntegers(char start, int end, String string) {
        int pos = 0;
        while (string.charAt(pos) != start) {
            pos++;
        }
        return getIntegers(pos, end, string);
    }

    public static List<Integer> getIntegers(char start, String string) {
        return getIntegers(start, string.length()-1, string);
    }

    public static List<Integer> getIntegers(String string) {
        return getIntegers(0, string.length(), string);
    }

    public static Character[] getNodes(String string) {
        Character[] nodes = new Character[2];
        nodes[0] = string.charAt(5);
        nodes[1] = string.charAt(36);
        return nodes;
    }

    public static List<DamageType> getWeaknesses(String s) {
        List<DamageType> weaknesses = new ArrayList<>();
        if (!s.contains("weak")) {
            return weaknesses;
        }
        int start = s.indexOf("weak to") + 8;
        int end;
        if (s.contains("(w") && s.contains(";")) {
            end = s.indexOf(";");
        } else {
            end = s.indexOf(")");
        }
        String[] weaknessesArr = s.substring(start, end).split("\\W+");
        for (String weakness : weaknessesArr) {
            weaknesses.add(stringToDamageType(weakness));
        }
        return weaknesses;
    }

    public static List<DamageType> getImmunities(String s) {
        List<DamageType> immunities = new ArrayList<>();
        if (!s.contains("immune")) {
            return immunities;
        }
        int start = s.indexOf("immune to") + 10;
        int end;
        if (s.contains("(i") && s.contains(";")) {
            end = s.indexOf(";");
        } else {
            end = s.indexOf(")");
        }
        String[] immunitiesArr = s.substring(start, end).split("\\W+");
        for (String immunity : immunitiesArr) {
            immunities.add(stringToDamageType(immunity));
        }
        return immunities;
    }

    public static DamageType stringToDamageType(String s) {
        switch (s) {
            case "bludgeoning":
                return DamageType.BLUDGEONING;
            case "cold":
                return DamageType.COLD;
            case "fire":
                return DamageType.FIRE;
            case "radiation":
                return DamageType.RADIATION;
            case "slashing":
                return DamageType.SLASHING;
            default:
                System.out.println("Unsupported damage type " + s);
        }
        return null;
    }

    public static DamageType getDamageType(String s) {
        int end = s.indexOf("damage") - 2;
        int start = end;
        for (; s.charAt(start) != ' '; start--);
        return stringToDamageType(s.substring(start + 1, end + 1));
    }
}
