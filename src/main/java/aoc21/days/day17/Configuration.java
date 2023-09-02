package aoc21.days.day17;

import aoc.math.geometry.Vector;

import java.util.Iterator;

public class Configuration implements Iterable<Vector> {
    private final int minX;
    private final int maxX;
    private final int minY;
    private final int maxY;

    public Configuration(TargetArea targetArea) {
        minX = Math.min(targetArea.getLowX(), 0);
        maxX = Math.max(targetArea.getHighX(), 0);
        if (targetArea.getLowY() < 0) {
            minY = targetArea.getLowY();
        } else {
            minY = (int) Math.ceil(-0.5 + Math.sqrt(0.25 + 2 * targetArea.getLowY()));
        }
        //Either miss on the way up or get to max/min X before starting to go down.
        int maxXDistance = Math.max(Math.abs(minX), Math.abs(maxX));
        maxY = Math.max(Math.max(targetArea.getHighY(), maxXDistance), Math.abs(targetArea.getLowY()));
    }

    private class VectorIterator implements Iterator<Vector> {
        private int x;
        private int y;

        public VectorIterator() {
            x = minX;
            y = maxY;
        }

        @Override
        public boolean hasNext() {
            return x != maxX || y != minY;
        }

        @Override
        public Vector next() {
            x++;
            if (x > maxX) {
                x = minX;
                y--;
            }
            return new Vector(x, y);
        }
    }

    @Override
    public Iterator<Vector> iterator() {
        return new VectorIterator();
    }
}
