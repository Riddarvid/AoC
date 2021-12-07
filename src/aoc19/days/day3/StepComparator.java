package aoc19.days.day3;

import java.util.Comparator;

public class StepComparator implements Comparator<Intersection> {
    @Override
    public int compare(Intersection i1, Intersection i2) {
        return (int)(i1.getSteps() - i2.getSteps());
    }
}
