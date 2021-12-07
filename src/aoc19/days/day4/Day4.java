package aoc19.days.day4;

import aoc19.utils.input.InputUtils;
import riddarvid.aoc.days.Day;

import java.util.List;

public class Day4 extends Day {
    private int low;
    private int high;

    public static void main(String[] args) {
        new Day4();
    }

    @Override
    protected void part1() {
        int nValid = 0;
        int pass = low;
        while (pass <= high) {
            if (isValid1(pass)) {
                nValid++;
            }
            pass++;
        }
        System.out.println(nValid);
    }

    private boolean isValid1(int pass) {
        return hasPair(pass) && isAscending(pass);
    }

    private boolean isAscending(int pass) {
        String password = Integer.toString(pass);
        for (int i = 0; i < password.length() - 1; i++) {
            if (password.charAt(i) > password.charAt(i + 1)) {
                return false;
            }
        }
        return true;
    }

    private boolean hasPair(int pass) {
        String password = Integer.toString(pass);
        for (int i = 0;i < password.length() - 1; i++) {
            if (password.charAt(i) == password.charAt(i + 1)) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected void part2() {
        int nValid = 0;
        int pass = low;
        while (pass <= high) {
            if (isValid2(pass)) {
                nValid++;
            }
            pass++;
        }
        System.out.println(nValid);
    }

    private boolean isValid2(int pass) {
        return hasStrictPair(pass) && isAscending(pass);
    }

    private boolean hasStrictPair(int pass) {
        String password = Integer.toString(pass);
        for (int i = 0;i < password.length() - 1; i++) {
            if (password.charAt(i) == password.charAt(i + 1)) {
                if (isStrictPair(password, i)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isStrictPair(String password, int i) {
        char potential = password.charAt(i);
        return (i <= 0 || password.charAt(i - 1) != potential) && (i >= password.length() - 2 || password.charAt(i + 2) != potential);
    }

    @Override
    protected void setup() {
        List<Integer> range = InputUtils.getInts(lines.get(0));
        low = range.get(0);
        high = range.get(1);
    }
}
