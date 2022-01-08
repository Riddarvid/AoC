package aoc15.days.day16;

import java.util.InputMismatchException;
import java.util.Objects;

public class Sample {
    private Compound compound;
    private final int count;

    public Sample(String compound, int count) {
        this(Compound.valueOf(compound.toUpperCase()), count);
    }

    public Sample(Compound compound, int count) {
        this.compound = compound;
        this.count = count;
    }

    public Compound getCompound() {
        return compound;
    }

    public int getCount() {
        return count;
    }

    @Override
    public String toString() {
        return compound.toString() + ", " + count;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Sample sample = (Sample) o;
        return compound == sample.compound;
    }

    @Override
    public int hashCode() {
        return Objects.hash(compound);
    }
}
