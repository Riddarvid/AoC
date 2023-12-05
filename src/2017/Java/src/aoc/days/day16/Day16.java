package aoc.days.day16;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;


public class Day16 extends Day {
    char[] dancers;
    List<DanceMove> moves;
    public static void main(String[] args) {
        new Day16();
    }

    @Override
    protected void part1() {
        dance();
        print(dancers);
    }

    private void print(char[] dancers) {
        for (char dancer : dancers) {
            System.out.print(dancer);
        }
        System.out.println();
    }

    @Override
    protected void part2() {
        reset();
        int cycleLength = findCycleLength(dancers);
        reset();
        int n = 1_000_000_000 % cycleLength;
        for (int i = 0; i < n; i++) {
            dance();
        }
        print(dancers);
    }

    private int findCycleLength(char[] dancers) {
        List<char[]> permutations = new ArrayList<>();
        permutations.add(Arrays.copyOf(dancers, dancers.length));
        do {
            dance();
            if (contains(permutations, dancers)) {
                break;
            }
            permutations.add(Arrays.copyOf(dancers, dancers.length));
        } while (true);
        return permutations.size() - permutations.indexOf(dancers) - 1;
    }

    private boolean contains(List<char[]> permutations, char[] dancers) {
        for (char[] arr : permutations) {
            if (Arrays.equals(arr, dancers)) {
                return true;
            }
        }
        return false;
    }

    private void dance() {
        for (DanceMove move : moves) {
            move.dance(dancers);
        }
    }

    private void reset() {
        for (int i = 0; i < dancers.length; i++) {
            dancers[i] = (char)('a' + i);
        }
    }

    @Override
    protected void setup() {
        dancers = new char[16];
        for (int i = 0; i < dancers.length; i++) {
            dancers[i] = (char)('a' + i);
        }
        moves = new ArrayList<>();
        List<String> tokens = InputUtilities.getTokens(lines.get(0), ',');
        for (String s : tokens) {
            DanceMove move;
            List<Integer> ints = InputUtilities.getInts(s);
            switch (s.charAt(0)) {
                case 's':
                    move = new Spin(ints.get(0));
                    break;
                case 'x':
                    move = new Exchange(ints.get(0), ints.get(1));
                    break;
                case 'p':
                    move = new Partner(s.charAt(1), s.charAt(3));
                    break;
                default:
                    throw new InputMismatchException(s);
            }
            moves.add(move);
        }
    }
}
