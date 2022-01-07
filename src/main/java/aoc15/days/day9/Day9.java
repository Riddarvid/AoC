package aoc15.days.day9;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day9 extends Day {

    private List<Location> locations;

    @Override
    public long part1() {
        int minDistance = Integer.MAX_VALUE;
        for (Location start : locations) {
            int current = start.getShortest();
            if (current < minDistance) {
                minDistance = current;
            }
        }
        return minDistance;
    }

    @Override
    public long part2() {
        int maxDistance = 0;
        for (Location start : locations) {
            int current = start.getLongest();
            if (current > maxDistance) {
                maxDistance = current;
            }
        }
        return maxDistance;
    }

    @Override
    public void setup() {
        locations = new ArrayList<>();
        for (String string : lines) {
            List<String> tokens = ParsingUtils.getTokens(string, ' ');
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
