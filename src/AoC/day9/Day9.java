package AoC.day9;

import AoC.FileUtilities;
import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day9 extends Day {
    public static void main(String[] args) {
        new Day9();
    }

    private List<Location> locations;

    @Override
    protected void part1() {
        int minDistance = Integer.MAX_VALUE;
        for (Location start : locations) {
            int current = start.getShortest();
            if (current < minDistance) {
                minDistance = current;
            }
        }
        System.out.println(minDistance);
    }

    @Override
    protected void part2() {
        int maxDistance = 0;
        for (Location start : locations) {
            int current = start.getLongest();
            if (current > maxDistance) {
                maxDistance = current;
            }
        }
        System.out.println(maxDistance);
    }

    @Override
    protected void setup() {
        locations = new ArrayList<>();
        for (String string : lines) {
            List<String> tokens = FileUtilities.getTokens(string, ' ');
            Location location = new Location(tokens.get(0));
            if (locations.contains(location)) {
                location = locations.get(locations.indexOf(location));
            } else {
                locations.add(location);
            }
            location.putDistance(tokens.get(2), Integer.parseInt(tokens.get(4)));
            location = new Location(tokens.get(2));
            if (locations.contains(location)) {
                location = locations.get(locations.indexOf(location));
            } else {
                locations.add(location);
            }
            location.putDistance(tokens.get(0), Integer.parseInt(tokens.get(4)));
        }
        Location.setLocations(locations);
    }
}
