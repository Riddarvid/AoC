package aoc21.days.day12;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Cave {
    private final String name;
    private final boolean isSmall;
    private final List<Cave> neighbours;

    public Cave(String name) {
        this.name = name;
        this.isSmall = Character.isLowerCase(name.charAt(0));
        neighbours = new ArrayList<>();
    }

    public void addNeighbour(Cave neighbour) {
        neighbours.add(neighbour);
    }

    public String getName() {
        return name;
    }

    public boolean isSmall() {
        return isSmall;
    }

    public List<Cave> getNeighbours() {
        return neighbours;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Cave cave = (Cave) o;
        return Objects.equals(name, cave.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
