package aoc.days.day10;

import java.util.List;
import java.util.Objects;

public class Asteroid {
    private final int x;
    private final int y;
    private int lineOfSights;

    public Asteroid(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public void calculateLineOfSights(List<Asteroid> asteroids) {
        lineOfSights = 0;
        for (Asteroid other : asteroids) {
            if (!this.equals(other)) {
                if (hasLineOfSight(other, asteroids)) {
                    lineOfSights++;
                }
            }
        }
    }

    private boolean hasLineOfSight(Asteroid other, List<Asteroid> asteroids) {

    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Asteroid asteroid = (Asteroid) o;
        return x == asteroid.x &&
                y == asteroid.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}
