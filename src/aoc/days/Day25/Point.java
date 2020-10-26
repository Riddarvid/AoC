package aoc.days.Day25;

public class Point {
    private final int x;
    private final int y;
    private final int z;
    private final int t;
    private boolean searched;

    public Point(int x, int y, int z, int t) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.t = t;
        searched = false;
    }

    public boolean isSearched() {
        return searched;
    }

    public void setSearched(boolean searched) {
        this.searched = searched;
    }

    @Override
    public String toString() {
        String s = "(" + x + ", " + y + ", " + z + ", " + t + ") ";
        if (searched) {
            s += "searched";
        } else {
            s += "not searched";
        }
        return s;
    }

    public boolean isInRangeOf(Point other) {
        return manhattan(this, other) <= 3;
    }

    public static int manhattan(Point p1, Point p2) {
        return Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z) + Math.abs(p1.t - p2.t);
    }
}
