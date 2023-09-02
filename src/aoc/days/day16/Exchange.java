package aoc.days.day16;

public class Exchange implements DanceMove {
    private final int posA;
    private final int posB;

    public Exchange(int posA, int posB) {
        this.posA = posA;
        this.posB = posB;
    }

    @Override
    public void dance(char[] dancers) {
        char temp = dancers[posA];
        dancers[posA] = dancers[posB];
        dancers[posB] = temp;
    }
}
