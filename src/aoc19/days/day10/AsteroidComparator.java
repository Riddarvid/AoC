package aoc19.days.day10;

import java.util.Comparator;

public class AsteroidComparator implements Comparator<Asteroid> {
    private Asteroid laser;

    public AsteroidComparator(Asteroid laser) {
        this.laser = laser;
    }

    @Override
    public int compare(Asteroid a1, Asteroid a2) {
        return (int)((laser.angleToUp(a1) - laser.angleToUp(a2)) * 10000);
    }
}
