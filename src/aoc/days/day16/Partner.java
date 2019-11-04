package aoc.days.day16;

import java.util.InputMismatchException;

public class Partner implements DanceMove {
    private final char programA;
    private final char programB;

    public Partner(char programA, char programB) {
        this.programA = programA;
        this.programB = programB;
    }

    @Override
    public void dance(char[] dancers) {
        int posA = indexOf(dancers, programA);
        int posB = indexOf(dancers, programB);
        dancers[posA] = programB;
        dancers[posB] = programA;
    }

    private int indexOf(char[] dancers, char c) {
        for (int i = 0; i < dancers.length; i++) {
            if (dancers[i] == c) {
                return i;
            }
        }
        throw new InputMismatchException();
    }
}
