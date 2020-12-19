package days.day17;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.math.geometry.PointND;

import java.util.HashSet;
import java.util.Set;

public class Day17 extends Day {
    Set<PointND> activeAtStart3D;
    Set<PointND> activeAtStart4D;

    public static void main(String[] args) {
        new Day17().runAndPrint();
    }

    @Override
    public long part1() {
        return runCycles(6, activeAtStart3D).size();
    }

    private Set<PointND> runCycles(int n, Set<PointND> activeAtStart) {
        Set<PointND> active = new HashSet<>(activeAtStart);
        for (int i = 0; i < n; i++) {
            active = runCycle(active);
        }
        return active;
    }

    private Set<PointND> runCycle(Set<PointND> active) {
        Set<PointND> toConsider = new HashSet<>();
        for (PointND point : active) {
            toConsider.add(point);
            toConsider.addAll(point.getNeighbours());
        }
        Set<PointND> newActive = new HashSet<>();
        for (PointND point : toConsider) {
            int activeNeighbours = countActiveNeighbours(point, active);
            if (active.contains(point) ) {
                if (activeNeighbours == 2 || activeNeighbours == 3) {
                    newActive.add(point);
                }
            } else {
                if (activeNeighbours == 3) {
                    newActive.add(point);
                }
            }
        }
        return newActive;
    }

    private int countActiveNeighbours(PointND point, Set<PointND> active) {
        Set<PointND> neighbours = point.getNeighbours();
        int sum = 0;
        for (PointND neighbour : neighbours) {
            if (active.contains(neighbour)) {
                sum++;
            }
        }
        return sum;
    }

    @Override
    public long part2() {
        return runCycles(6, activeAtStart4D).size();
    }

    @Override
    public void setup() {
        activeAtStart3D = new HashSet<>();
        activeAtStart4D = new HashSet<>();
        for (int y = 0; y < lines.size(); y++) {
            String row = lines.get(y);
            for (int x = 0; x < row.length(); x++) {
                if (row.charAt(x) == '#') {
                    activeAtStart3D.add(new PointND(3, x, y, 0));
                    activeAtStart4D.add(new PointND(4, x, y, 0, 0));
                }
            }
        }
    }
}
