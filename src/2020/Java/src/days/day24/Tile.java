package days.day24;

import riddarvid.aoc.math.geometry.Point;

import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class Tile {
    private final Point point;

    public Tile(Point point) {
        this.point = point;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Tile tile = (Tile) o;
        return Objects.equals(point, tile.point);
    }

    @Override
    public int hashCode() {
        return Objects.hash(point);
    }

    public Set<Tile> getNeighbours() {
        Set<Tile> neighbours = new HashSet<>();
        neighbours.add(new Tile(point.moveBy(Direction.eastVector)));
        neighbours.add(new Tile(point.moveBy(Direction.southEastVector)));
        neighbours.add(new Tile(point.moveBy(Direction.southWestVector)));
        neighbours.add(new Tile(point.moveBy(Direction.westVector)));
        neighbours.add(new Tile(point.moveBy(Direction.northWestVector)));
        neighbours.add(new Tile(point.moveBy(Direction.northEastVector)));
        return neighbours;
    }
}
