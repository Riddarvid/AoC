package aoc15.days.day20;

import aoc.days.Day;

import java.util.*;

public class Day20 extends Day {
    private int min;

    @Override
    public long part1() {
        Map<Integer, Set<Integer>> elvesMap = new HashMap<>();
        int houseNumber = 1;
        while (presentsToHouse1(houseNumber, elvesMap) < min) {
            houseNumber++;
        }
        return houseNumber;
    }

    private int presentsToHouse1(int houseNumber, Map<Integer, Set<Integer>> elvesMap) {
        for (int i = 2; i <= Math.sqrt(houseNumber); i++) {
            if (houseNumber % i == 0) { //i is a factor of houseNumber
                Set<Integer> partFactors = elvesMap.get(houseNumber / i);
                Set<Integer> elves = getProductFactors1(partFactors, i);
                elvesMap.put(houseNumber, elves);
                return getPresentsFromElves1(elves);
            }
        }
        Set<Integer> elves = new HashSet<>();
        elves.add(1);
        elves.add(houseNumber);
        elvesMap.put(houseNumber, elves);
        return getPresentsFromElves1(elves);
    }

    private int getPresentsFromElves1(Set<Integer> factors) {
        int sum = 0;
        for (int factor : factors) {
            sum += factor;
        }
        sum *= 10;
        return sum;
    }

    private int getPresentsFromElves2(Set<Integer> elves) {
        int sum = 0;
        for (int factor : elves) {
            sum += factor;
        }
        sum *= 11;
        return sum;
    }

    private Set<Integer> getProductFactors1(Set<Integer> partFactors, int factor) {
        Set<Integer> factors = new HashSet<>(partFactors);
        for (int partFactor : partFactors) {
            factors.add(partFactor * factor);
        }
        return factors;
    }

    @Override
    public long part2() {
        Map<Integer, Set<Integer>> elvesMap = new HashMap<>();
        int houseNumber = 1;
        while (presentsToHouse2(houseNumber, elvesMap) < min) {
            houseNumber++;
        }
        return houseNumber;
    }

    private int presentsToHouse2(int houseNumber, Map<Integer, Set<Integer>> elvesMap) {
        for (int i = 2; i <= Math.sqrt(houseNumber); i++) {
            if (houseNumber % i == 0) { //i is a factor of houseNumber
                Set<Integer> partFactors = elvesMap.get(houseNumber / i);
                Set<Integer> elves = getProductFactors2(partFactors, i, houseNumber);
                elvesMap.put(houseNumber, elves);
                return getPresentsFromElves2(elves);
            }
        }
        Set<Integer> elves = new HashSet<>();
        if (houseNumber <= 50) {
            elves.add(1);
        }
        elves.add(houseNumber);
        elvesMap.put(houseNumber, elves);
        return getPresentsFromElves2(elves);
    }

    private Set<Integer> getProductFactors2(Set<Integer> partFactors, int factor, int houseNumber) {
        Set<Integer> potentialFactors = getProductFactors1(partFactors, factor);
        Set<Integer> factors = new HashSet<>();
        for (int potentialFactor : potentialFactors) {
            if (potentialFactor * 50 >= houseNumber) {
                factors.add(potentialFactor);
            }
        }
        return factors;
    }

    @Override
    public void setup() {
        min = Integer.parseInt(lines.get(0));
    }
}
