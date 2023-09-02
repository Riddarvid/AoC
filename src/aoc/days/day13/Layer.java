package aoc.days.day13;

public class Layer {
    private final int depth;
    private final Scanner scanner;

    public Layer(int depth, int range) {
        this.depth = depth;
        scanner = new Scanner(range);
    }

    public int getDepth() {
        return depth;
    }

    public int getScannerDepth() {
        return scanner.getPosition();
    }

    public int getRange() {
        return scanner.getRange();
    }

    public void moveScanner() {
        scanner.move();
    }

    public void resetScanner() {
        scanner.resetPosition();
    }
}
