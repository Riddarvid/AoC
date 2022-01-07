package aoc15.days.day16;

import java.util.InputMismatchException;
import java.util.Objects;

public class Sample {
    private Compound compound;
    private final int count;

    public Sample(String compound, int count) {
        setCompound(compound);
        this.count = count;
    }

    private void setCompound(String string) {
        switch (string) {
            case "children":
                compound = Compound.CHILDREN;
                break;
            case "cats":
                compound = Compound.CATS;
                break;
            case "samoyeds":
                compound = Compound.SAMOYEDS;
                break;
            case "pomeranians":
                compound = Compound.POMERANIANS;
                break;
            case "akitas":
                compound = Compound.AKITAS;
                break;
            case "vizslas":
                compound = Compound.VIZSLAS;
                break;
            case "goldfish":
                compound = Compound.GOLDFISH;
                break;
            case "trees":
                compound = Compound.TREES;
                break;
            case "cars":
                compound = Compound.CARS;
                break;
            case "perfumes":
                compound = Compound.PERFUMES;
                break;
            default:
                throw new InputMismatchException("Invalid compound");
        }
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
