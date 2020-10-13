package AoC.day20;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day20 extends Day {
    private int min;

    public static void main(String[] args) {
        new Day20();
    }

    @Override
    protected void part1() {
        int houseNumber = 1;
        while (presentsToHouse(houseNumber) < min) {
            houseNumber++;
        }
        System.out.println(houseNumber);
    }

    private int presentsToHouse(int houseNumber) {
        int sum = 0;
        List<Integer> factors = getFactors(houseNumber);
        for (int factor : factors) {
            sum += factor;
        }
        sum *= 10;
        return sum;
    }

    private List<Integer> getFactors(int houseNumber) {
        List<Integer> factors = new ArrayList<>();
        for (int i = 1; i <= houseNumber; i++) {
            if (houseNumber % i == 0) {
                factors.add(i);
            }
        }
        return factors;
    }

    @Override
    protected void part2() {
        /*int houseNumber = 1;
        while (presentsToHouse2(houseNumber) < min1) {
            houseNumber++;
        }
        System.out.println(houseNumber);*/
    }

    private int presentsToHouse2(int houseNumber) {
        List<Integer> factors = getFactors(houseNumber);
        int sum = 0;
        for (int factor : factors) {
            if (houseNumber / factor >= 50) {
                sum += factor;
            }
        }
        sum *= 11;
        return sum;
    }

    @Override
    protected void setup() {
        min = Integer.parseInt(lines.get(0));
    }
}
