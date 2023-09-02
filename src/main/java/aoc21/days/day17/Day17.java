package aoc21.days.day17;

import aoc.days.Day;
import aoc.math.geometry.Vector;
import aoc.parsing.ParsingUtils;

import java.util.List;

public class Day17 extends Day {
    private TargetArea targetArea;
    private Configuration configuration;

    @Override
    public void setup() {
        List<Integer> coordinates = ParsingUtils.getIntegersNegative(lines.get(0));
        targetArea = new TargetArea(coordinates.get(0), coordinates.get(1), coordinates.get(2), coordinates.get(3));
        configuration = new Configuration(targetArea);
    }

    public int getHighestY(Vector startVelocity) {
        //Highest point is n + (n-1) + (n-2) +...
        int n = startVelocity.getY();
        return n * (n + 1) / 2;
    }

    public Vector getHighestYVelocity(TargetArea targetArea) {
        for (Vector startVelocity : configuration) {
            if (canHitTarget(startVelocity, targetArea)) {
                return startVelocity;
            }
        }
        throw new IllegalArgumentException("No valid configuration found.");
    }

    private boolean canHitTarget(Vector startVelocity, TargetArea targetArea) {
        Probe probe = new Probe(startVelocity);
        while (true) {
            if (targetArea.contains(probe)) {
                return true;
            }
            if (targetArea.hasBeenPassed(probe)) {
                return false;
            }
            probe.move();
        }
    }

    @Override
    public long part1() {
        Vector startVelocity = getHighestYVelocity(targetArea);
        return getHighestY(startVelocity);
    }

    private int getNumberOfHits(TargetArea targetArea) {
        int sum = 0;
        for (Vector vector : configuration) {
            if (canHitTarget(vector, targetArea)) {
                sum++;
            }
        }
        return sum;
    }

    @Override
    public long part2() {
        return getNumberOfHits(targetArea);
    }
}
