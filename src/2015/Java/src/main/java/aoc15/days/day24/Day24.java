package aoc15.days.day24;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class Day24 extends Day {
    private int goalWeight1;
    private int goalWeight2;
    private List<Integer> packages;

    @Override
    public long part1() {
        List<Integer> leastQE = findLeastQE(goalWeight1, packages);
        return getQE(leastQE);
    }

    private long getQE(List<Integer> leastQE) {
        long product = 1;
        for (int term : leastQE) {
            product *= term;
        }
        return product;
    }

    private List<Integer> findLeastQE(int goalWeight, List<Integer> packages) {
        int size = 1;
        List<Integer> leastQE = findLeastQE(goalWeight, packages, new ArrayList<>(), size);;
        while (leastQE == null) {
            size++;
            leastQE = findLeastQE(goalWeight, packages, new ArrayList<>(), size);
        }
        return leastQE;
    }

    private List<Integer> findLeastQE(int goalWeight, List<Integer> packages, List<Integer> soFar, int size) {
        if (soFar.size() == size) {
            if (weight(soFar) == goalWeight) {
                return soFar;
            }
            return null;
        }
        int startIndex;
        if (soFar.isEmpty()) {
            startIndex = 0;
        } else {
            startIndex = packages.indexOf(soFar.get(soFar.size() - 1)) + 1;
        }
        for (int i = startIndex; i < packages.size(); i++) {
            Integer nextTerm = packages.get(i);
            List<Integer> newSoFar = new ArrayList<>(soFar);
            newSoFar.add(nextTerm);
            if (weight(newSoFar) > goalWeight) {
                return null;
            }
            List<Integer> potentialLeastQE = findLeastQE(goalWeight, packages, newSoFar, size);
            if (potentialLeastQE != null) {
                return potentialLeastQE;
            }
        }
        return null;
    }

    private int weight(Collection<Integer> soFar) {
        int sum = 0;
        for (int present : soFar) {
            sum += present;
        }
        return sum;
    }

    @Override
    public long part2() {
        //List<Integer> smallest = findSmallest(goalWeight2, packages);
        List<Integer> leastQE = findLeastQE(goalWeight2, packages);
        return getQE(leastQE);
    }

    @Override
    public void setup() {
        packages = new ArrayList<>();
        int totalWeight = 0;
        for (String s : lines) {
            int weight = Integer.parseInt(s);
            packages.add(weight);
            totalWeight += weight;
        }
        goalWeight1 = totalWeight / 3;
        goalWeight2 = totalWeight / 4;
    }
}
