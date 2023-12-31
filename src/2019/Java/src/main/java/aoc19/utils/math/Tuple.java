package aoc19.utils.math;

import java.util.Objects;

public class Tuple <T, U> {
    private final T first;
    private final U second;

    public T getFirst() {
        return first;
    }

    public U getSecond() {
        return second;
    }

    public Tuple(T first, U second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Tuple<?, ?> tuple = (Tuple<?, ?>) o;
        return Objects.equals(first, tuple.first) &&
                Objects.equals(second, tuple.second);
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }

    @Override
    public String toString() {
        return first.toString() + ", " + second.toString();
    }
}

