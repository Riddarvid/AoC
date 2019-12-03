package aoc.days.day3;

import java.util.Comparator;

public class IntersectionComparator implements Comparator<Intersection> {

    @Override
    public int compare(Intersection i1, Intersection i2) {
        Point origo = new Point(0, 0);
        return i1.getPoint().distanceFrom(origo) - i2.getPoint().distanceFrom(origo);
    }
}
