package aoc.days.day03;

import aoc.utils.geometry.Point2D;

import java.util.Comparator;

public class IntersectionComparator implements Comparator<Intersection> {

    @Override
    public int compare(Intersection i1, Intersection i2) {
        Point2D origo = new Point2D(0, 0);
        return (int)(i1.getPoint().distanceFrom(origo) - i2.getPoint().distanceFrom(origo));
    }
}
