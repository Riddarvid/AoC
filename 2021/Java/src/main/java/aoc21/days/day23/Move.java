package aoc21.days.day23;

import java.util.Objects;

public class Move {
    private final Burrow resultBurrow;
    private final int cost;

    public Move(Burrow resultBurrow, int cost) {
        this.resultBurrow = resultBurrow;
        this.cost = cost;
    }

    public Burrow getResultBurrow() {
        return resultBurrow;
    }

    public int getCost() {
        return cost;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Move move = (Move) o;
        return cost == move.cost && Objects.equals(resultBurrow, move.resultBurrow);
    }

    @Override
    public int hashCode() {
        return Objects.hash(resultBurrow, cost);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(resultBurrow).append(" Cost: ").append(cost);
        return sb.toString();
    }
}
