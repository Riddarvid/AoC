package aoc21.days.day22;

import aoc.math.geometry.PointND;

public class Cuboid {
    private final PointND lowCoordinate;
    private final PointND highCoordinate;

    public Cuboid(int x1, int x2, int y1, int y2, int z1, int z2) {
        int lowX = Math.min(x1, x2);
        int highX = Math.max(x1, x2);
        int lowY = Math.min(y1, y2);
        int highY = Math.max(y1, y2);
        int lowZ = Math.min(z1, z2);
        int highZ = Math.max(z1, z2);
        lowCoordinate = new PointND(lowX, lowY, lowZ);
        highCoordinate = new PointND(highX, highY, highZ);
    }

    public long size() {
        long sideX = highCoordinate.getCoordinate(0) - lowCoordinate.getCoordinate(0) + 1;
        long sideY = highCoordinate.getCoordinate(1) - lowCoordinate.getCoordinate(1) + 1;
        long sideZ = highCoordinate.getCoordinate(2) - lowCoordinate.getCoordinate(2) + 1;
        return sideX * sideY * sideZ;
    }

    public boolean overlaps(Cuboid other) {
        for (int dimension = 0; dimension < 3; dimension++) {
            if (!overlapsInDimension(other, dimension)) {
                return false;
            }
        }
        return true;
    }

    private boolean overlapsInDimension(Cuboid other, int dimension) {
        return this.highCoordinate.getCoordinate(dimension) >= other.lowCoordinate.getCoordinate(dimension) &&
                other.highCoordinate.getCoordinate(dimension) >= this.lowCoordinate.getCoordinate(dimension);
    }

    public Cuboid getOverlap(Cuboid other) {
        int lowX  = Math.max(this.lowCoordinate.getCoordinate(0), other.lowCoordinate.getCoordinate(0));
        int highX = Math.min(this.highCoordinate.getCoordinate(0), other.highCoordinate.getCoordinate(0));
        int lowY  = Math.max(this.lowCoordinate.getCoordinate(1), other.lowCoordinate.getCoordinate(1));
        int highY = Math.min(this.highCoordinate.getCoordinate(1), other.highCoordinate.getCoordinate(1));
        int lowZ  = Math.max(this.lowCoordinate.getCoordinate(2), other.lowCoordinate.getCoordinate(2));
        int highZ = Math.min(this.highCoordinate.getCoordinate(2), other.highCoordinate.getCoordinate(2));
        return new Cuboid(lowX, highX, lowY, highY, lowZ, highZ);
    }
}
