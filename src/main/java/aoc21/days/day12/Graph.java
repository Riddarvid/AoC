package aoc21.days.day12;

import java.util.*;

public class Graph {
    private Cave start;
    private Cave end;
    private final Set<Cave> allCaves;

    public Graph(List<String> lines) {
        allCaves = new HashSet<>();
        Map<String, Cave> nameMap = new HashMap<>();
        for (String line : lines) { //Create all caves
            String[] names = line.split("-");
            Cave cave1;
            Cave cave2;
            if (!nameMap.containsKey(names[0])) {
                cave1 = new Cave(names[0]);
                nameMap.put(names[0], cave1);
            } else {
                cave1 = nameMap.get(names[0]);
            }
            if (!nameMap.containsKey(names[1])) {
                cave2 = new Cave(names[1]);
                nameMap.put(names[1], cave2);
            } else {
                cave2 = nameMap.get(names[1]);
            }
            allCaves.add(cave1);
            allCaves.add(cave2);
            //Assign start and end
            if (cave1.getName().equals("start")) {
                start = cave1;
            } else if (cave1.getName().equals("end")) {
                end = cave1;
            }
            if (cave2.getName().equals("start")) {
                start = cave2;
            } else if (cave2.getName().equals("end")) {
                end = cave2;
            }
            //Connect caves
            cave1.addNeighbour(cave2);
            cave2.addNeighbour(cave1);
        }
    }

    public long getNumberOfPathsImproved(int method) {
        Map<Cave, Integer> caveCount = new HashMap<>();
        for (Cave cave : allCaves) {
            caveCount.put(cave, 0);
        }
        ValidityFunction validityFunction;
        if (method == 1) {
            validityFunction = this::isValidSmallOnce;
        } else {
            validityFunction = this::isValidOneSmallTwice;
        }
        return getNumberOfPathsImproved(start, caveCount, validityFunction);
    }

    public long getNumberOfPathsImproved(Cave position, Map<Cave, Integer> caveCount, ValidityFunction validityFunction) {
        int count = caveCount.get(position);
        if (position == end) { //End reached.
            return 1;
        }
        if (position == start && count > 0) {
            return 0;
        }
        caveCount.put(position, count + 1); //Add 1 to account for the current cave.
        if (position.isSmall() && !validityFunction.isValid(caveCount)) { //Small cave visited more than allowed.
            caveCount.put(position, count); //Reset
            return 0;
        }
        long numberOfPaths = 0;
        for (Cave neighbour : position.getNeighbours()) {
            numberOfPaths += getNumberOfPathsImproved(neighbour, caveCount, validityFunction);
        }
        caveCount.put(position, count); //Reset count after leaving cave.
        return numberOfPaths;
    }

    private boolean isValidSmallOnce(Map<Cave, Integer> caveCount) {
        for (Cave cave : caveCount.keySet()) {
            if (cave.isSmall() && caveCount.get(cave) > 1) {
                return false;
            }
        }
        return true;
    }

    private boolean isValidOneSmallTwice(Map<Cave, Integer> caveCount) {
        boolean smallVisitedTwice = false;
        for (Cave cave : caveCount.keySet()) {
            if (cave.isSmall()) {
                int count = caveCount.get(cave);
                if (count > 2) {
                    return false;
                }
                if (smallVisitedTwice && count > 1) {
                    return false;
                }
                if (count > 1) {
                    smallVisitedTwice = true;
                }
            }
        }
        return true;
    }
}
