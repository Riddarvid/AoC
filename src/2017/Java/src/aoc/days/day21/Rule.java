package aoc.days.day21;

import java.util.Arrays;

public class Rule {
    private char[][][] inPatterns;
    private char[][] outPattern;

    public Rule(char[][] in, char[][] out) {
        inPatterns = getVariations(in);
        outPattern = out;
    }

    private char[][][] getVariations(char[][] in) {
        char[][][] variations = new char[8][][];
        variations[0] = in;
        char[][] next = in;
        for (int i = 1; i < 4; i++) {
            next = rotate(next);
            variations[i] = next;
        }
        next = flip(next);
        variations[4] = next;
        for (int i = 5; i < 8; i++) {
            next = rotate(next);
            variations[i] = next;
        }
        return variations;
    }

    private char[][] rotate(char[][] next) {
        char[][] rotated = new char[next.length][next[0].length];
        for (int i = 0; i < next.length; i++) {
            for (int j = 0; j <next[0].length; j++) {
                rotated[i][j] = next[j][next.length - i - 1];
            }
        }
        return rotated;
    }

    private char[][] flip(char[][] next) {
        char[][] flipped = new char[next.length][next.length];
        for (int i = 0; i < next.length; i++) {
            for (int j = 0; j <next.length; j++) {
                flipped[i][j] = next[i][next.length - j - 1];
            }
        }
        return flipped;
    }

    public boolean matches(char[][] pattern) {
        for (char[][] p : inPatterns) {
            if (Arrays.deepEquals(p, pattern)) {
                return true;
            }
        }
        return false;
    }

    public char[][] getOutPattern() {
        return outPattern;
    }
}
