package aoc.days.day11;

import aoc.utils.geometry.Point2D;

import java.util.Objects;

public class Pair implements Comparable<Pair> {
    private Point2D positions;

    public Pair() {
        positions = new Point2D(0, 0);
    }

    public Pair(int generator, int microchip) {
        positions = new Point2D(generator, microchip);
    }

    public void setGenerator(int generator) {
        positions = new Point2D(generator, positions.getY());
    }

    public void setMicrochip(int microchip) {
        positions = new Point2D(positions.getX(), microchip);
    }

    public int getGenerator() {
        return (int)positions.getX();
    }

    public int getMicrochip() {
        return (int)positions.getY();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pair pair = (Pair) o;
        return Objects.equals(positions, pair.positions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(positions);
    }

    @Override
    public String toString() {
        return "G: " + getGenerator() + " M: " + getMicrochip();
    }

    public Pair moveMicrochip(Direction direction) {
        if (direction == Direction.UP) {
            return new Pair(getGenerator(), getMicrochip() + 1);
        } else {
            return new Pair(getGenerator(), getMicrochip() - 1);
        }
    }

    public Pair moveGenerator(Direction direction) {
        if (direction == Direction.UP) {
            return new Pair(getGenerator() + 1, getMicrochip());
        } else {
            return new Pair(getGenerator() - 1, getMicrochip());
        }
    }

    @Override
    public int compareTo(Pair pair) {
        if (this.positions.getX() != pair.positions.getX()) {
            return (int)(this.positions.getX() - pair.positions.getX());
        }
        return (int)(this.positions.getY() - pair.positions.getY());
    }
}
