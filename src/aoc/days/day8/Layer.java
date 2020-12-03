package aoc.days.day8;

public class Layer {
    private int[][] pixels;

    public Layer(int[][] pixels) {
        this.pixels = pixels;
    }

    public int leastZeros() {
        int sum = 0;
        for (int[] row : pixels) {
            sum += numberOfRow(0, row);
        }
        return sum;
    }

    private int numberOfRow(int target, int[] row) {
        int sum = 0;
        for (int i : row) {
            if (target == i) {
                sum++;
            }
        }
        return sum;
    }

    private int numberOf(int target) {
        int sum = 0;
        for (int[] row : pixels) {
            sum += numberOfRow(target, row);
        }
        return sum;
    }

    public int checksum() {
        return numberOf(1) * numberOf(2);
    }

    public int get(int row, int pos) {
        return pixels[row][pos];
    }
}
