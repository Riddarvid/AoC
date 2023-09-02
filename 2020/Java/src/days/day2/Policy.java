package days.day2;

public abstract class Policy {
    private final char letter;
    private final int low;
    private final int high;

    public Policy(char letter, int low, int high) {
        this.letter = letter;
        this.low = low;
        this.high = high;
    }

    public abstract boolean fulfills(String password);

    public char getLetter() {
        return letter;
    }

    public int getLow() {
        return low;
    }

    public int getHigh() {
        return high;
    }
}
