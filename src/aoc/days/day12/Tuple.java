package aoc.days.day12;

import java.util.Objects;

public class Tuple {
    private final long position;
    private final long velocity;


    public Tuple(long position, long velocity) {
        this.position = position;
        this.velocity = velocity;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Tuple tuple = (Tuple) o;
        return position == tuple.position &&
                velocity == tuple.velocity;
    }

    @Override
    public int hashCode() {
        return Objects.hash(position, velocity);
    }
}
