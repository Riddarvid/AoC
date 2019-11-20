package AoC.day9;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class Location {
    private Map<String, Integer> distances;
    private static List<Location> locations;
    private String name;
    private boolean visited = false;

    public Location(String name) {
        this.name = name;
        distances = new HashMap<>();
        distances.put(name, 0);
    }

    public void putDistance(String location, int distance) {
        distances.put(location, distance);
    }

    public int getDistance(String location) {
        return distances.get(location);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Location location = (Location) o;
        return Objects.equals(name, location.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }

    public int getShortest() {
        visited = true;
        int minDistance = Integer.MAX_VALUE;
        for (Location other : locations) {
            if (!other.isVisited()) {
                int current = other.getShortest() + distances.get(other.name);
                if (current < minDistance) {
                    minDistance = current;
                }
            }
        }
        if (minDistance == Integer.MAX_VALUE) {
            minDistance = 0;
        }
        visited = false;
        return minDistance;
    }

    public int getLongest() {
        visited = true;
        int maxDistance = 0;
        for (Location other : locations) {
            if (!other.isVisited()) {
                int current = other.getLongest() + distances.get(other.name);
                if (current > maxDistance) {
                    maxDistance = current;
                }
            }
        }
        visited = false;
        return maxDistance;
    }

    public static void setLocations(List<Location> locations) {
        Location.locations = locations;
    }

    public boolean isVisited() {
        return visited;
    }
}
