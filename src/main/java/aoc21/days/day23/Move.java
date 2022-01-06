package aoc21.days.day23;

public class Move {
    private final Burrow burrow;
    private final int cost;

    public Move(Burrow burrow, int cost) {
        this.burrow = burrow;
        this.cost = cost;
    }

    public Burrow getBurrow() {
        return burrow;
    }

    public int getCost() {
        return cost;
    }
}
