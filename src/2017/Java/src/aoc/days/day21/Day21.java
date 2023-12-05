package aoc.days.day21;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day21 extends Day {
    private List<Rule> rules;
    char[][] current;

    public static void main(String[] args) {
        new Day21();
    }

    @Override
    protected void part1() {
        run(5);
        System.out.println(countOn());
    }

    private void run(int steps) {
        for (int i = 0; i < steps; i++) {
            step();
            //printMatrix(current);
        }
    }

    private int countOn() {
        int sum = 0;
        for (int i = 0; i < current.length; i++) {
            for (int j = 0; j < current.length; j++) {
                if (current[i][j] == '#') {
                    sum++;
                }
            }
        }
        return sum;
    }

    private void printMatrix(char[][] current) {
        for (int i = 0; i < current.length; i++) {
            System.out.println(String.valueOf(current[i]));
        }
        System.out.println();
    }

    private void step() {
        char[][] next;
        int currentLength;
        if (current.length % 2 == 0) {
            currentLength = 2;
        } else {
            currentLength = 3;
        }
        int nextLength = currentLength + 1;
        next = new char[nextLength * (current.length / currentLength)][nextLength * (current.length / currentLength)];
        for (int i = 0; i < current.length / currentLength; i ++) {
            for (int j = 0; j < current.length / currentLength; j ++) {
                char[][] in = getSubMatrix(current, i * currentLength, j * currentLength, currentLength);
                char[][] out = getOut(in);
                insert(out, next, i * nextLength, j * nextLength);
            }
        }
        current = next;
    }

    private void insert(char[][] toInsert, char[][] next, int y, int x) {
        for (int i = 0; i < toInsert.length; i++) {
            for (int j = 0; j < toInsert.length; j++) {
                next[y + i][x + j] = toInsert[i][j];
            }
        }
    }

    private char[][] getOut(char[][] in) {
        for (Rule rule : rules) {
            if (rule.matches(in)) {
                return rule.getOutPattern();
            }
        }
        throw new InputMismatchException();
    }

    private char[][] getSubMatrix(char[][] current, int y, int x, int length) {
        char[][] out = new char[length][length];
        for (int i = 0; i < length; i++) {
            for (int j = 0; j < length; j++) {
                out[i][j] = current[y + i][x + j];
            }
        }
        return out;
    }

    @Override
    protected void part2() {
        run(13);
        System.out.println(countOn());
    }

    @Override
    protected void setup() {
        rules = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, '=');
            String in = tokens.get(0).substring(0, tokens.get(0).length() - 1);
            String out = tokens.get(1).substring(2);
            char[][] inPattern = stringToPattern(in);
            char[][] outPattern = stringToPattern(out);
            rules.add(new Rule(inPattern, outPattern));
        }
        current = stringToPattern(".#./..#/###");
    }

    private char[][] stringToPattern(String in) {
        char[][] pattern;
        if (in.length() == 5) {
            pattern = new char[2][2];
        } else if (in.length() == 11){
            pattern = new char[3][3];
        } else {
            pattern = new char[4][4];
        }
        in = in.replace("/", "");
        for (int i = 0; i < pattern.length; i++) {
            for (int j = 0; j < pattern.length; j++) {
                pattern[i][j] = in.charAt(i * pattern.length + j);
            }
        }
        return pattern;
    }
}
