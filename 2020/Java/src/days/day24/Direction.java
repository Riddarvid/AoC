package days.day24;

import riddarvid.aoc.math.geometry.Vector;

public enum Direction {
    EAST, SOUTH_EAST, SOUTH_WEST, WEST, NORTH_WEST, NORTH_EAST;

    public static final Vector eastVector      = new Vector(2, 0);
    public static final Vector southEastVector = new Vector(1, -1);
    public static final Vector southWestVector = new Vector(-1, -1);
    public static final Vector westVector      = new Vector(-2, 0);
    public static final Vector northWestVector = new Vector(-1, 1);
    public static final Vector northEastVector = new Vector(1, 1);
}
