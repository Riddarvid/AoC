package aoc.days.day22;

import java.util.Objects;

public class Permutation {
    private final int infectedHash;
    private final int weakenedHash;
    private final int flaggedHash;
    private final int x;
    private final int y;
    private final Direction direction;

    public Permutation(int infectedHash, int weakenedHash, int flaggedHash, int x, int y, Direction direction) {
        this.infectedHash = infectedHash;
        this.weakenedHash = weakenedHash;
        this.flaggedHash = flaggedHash;
        this.x = x;
        this.y = y;
        this.direction = direction;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Permutation that = (Permutation) o;
        return infectedHash == that.infectedHash &&
                weakenedHash == that.weakenedHash &&
                flaggedHash == that.flaggedHash &&
                x == that.x &&
                y == that.y &&
                direction == that.direction;
    }

    @Override
    public int hashCode() {
        return Objects.hash(infectedHash, weakenedHash, flaggedHash, x, y, direction);
    }
}
