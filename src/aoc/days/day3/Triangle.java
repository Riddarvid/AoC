package aoc.days.day3;

public class Triangle {
    private final int longest;
    private final int side1;
    private final int side2;

    public Triangle(int side1, int side2, int side3) {
        if (side1 >= side2 && side1 >= side3) {
            longest = side1;
            this.side1 = side2;
            this.side2 = side3;
        } else if (side2 >= side1 && side2 >= side3) {
            longest = side2;
            this.side1 = side1;
            this.side2 = side3;
        } else {
            longest = side3;
            this.side1 = side1;
            this.side2 = side2;
        }
    }

    public boolean isValid() {
        return longest < side1 + side2;
    }
}
