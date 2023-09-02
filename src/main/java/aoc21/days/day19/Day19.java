package aoc21.days.day19;

import aoc.days.Day;
import aoc.math.geometry.PointND;
import aoc.math.geometry.VectorND;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day19 extends Day {
    private List<Scanner> scanners;

    @Override
    public void setup() {
        scanners = new ArrayList<>();
        int index = 1;
        int id = 0;
        while (index < lines.size()) {
            List<PointND> points = new ArrayList<>();
            while (index < lines.size() && !lines.get(index).equals("")) {
                List<Integer> values = ParsingUtils.getIntegersNegative(lines.get(index));
                points.add(new PointND(values.get(0), values.get(1), values.get(2)));
                index++;
            }
            scanners.add(new Scanner(points, id));
            id++;
            index += 2;
        }
    }

    private void determinePositions() {
        Scanner referenceScanner = scanners.get(0);
        referenceScanner.setAsReferenceScanner();
        List<Scanner> remaining = new ArrayList<>(scanners);
        remaining.remove(referenceScanner);
        List<Scanner> discoveredLastTurn = new ArrayList<>();
        discoveredLastTurn.add(referenceScanner);
        while (!discoveredLastTurn.isEmpty()) {
            List<Scanner> discoveredThisTurn = new ArrayList<>();
            for (Scanner scanner : discoveredLastTurn) {
                List<Scanner> toRemove = new ArrayList<>();
                for (Scanner other : remaining) {
                    if (scanner.tryOverlap(other)) {
                        discoveredThisTurn.add(other);
                        toRemove.add(other);
                    }
                }
                remaining.removeAll(toRemove);
            }
            discoveredLastTurn = discoveredThisTurn;
        }
    }

    @Override
    public long part1() {
        determinePositions();
        Set<PointND> allPoints = new HashSet<>();
        for (Scanner scanner : scanners) {
            allPoints.addAll(scanner.getDeterminedBeacons());
        }
        return allPoints.size();
    }

    @Override
    public long part2() {
        long maxDistance = 0;
        for (int i = 0; i < scanners.size(); i++) {
            for (int j = i + 1; j < scanners.size(); j++) {
                PointND position1 = scanners.get(i).getPosition();
                PointND position2 = scanners.get(j).getPosition();
                VectorND vector = position1.vectorTo(position2);
                long distance = vector.manhattanLength();
                if (distance > maxDistance) {
                    maxDistance = distance;
                }
            }
        }
        return maxDistance;
    }
}
