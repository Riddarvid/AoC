package aoc19.days.day6;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Planet {
    private final String id;
    private Planet orbits;

    public Planet(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setOrbits(Planet orbits) {
        this.orbits = orbits;
    }

    public List<Planet> getPathToCom() {
        List<Planet> path = new ArrayList<>();
        getPathToCom(path);
        return path;
    }

    public void getPathToCom(List<Planet> path) {
        if (orbits != null) {
            orbits.getPathToCom(path);
        }
        path.add(this);
    }

    public int getOrbits() {
        if (orbits != null) {
            return 1 + orbits.getOrbits();
        } else {
            return 0;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Planet planet = (Planet) o;
        return Objects.equals(id, planet.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
