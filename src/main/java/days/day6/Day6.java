package days.day6;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.List;

public class Day6 extends Day {
    long[] fishGroups;

    public static void main(String[] args) {
        new Day6().runAndPrint();
    }

    public Day6() {
        super("input6");
    }

    private long simulate(int turns, long[] fishGroups) {
        for (int i = 0; i < turns; i++) {
            simulateTurn(fishGroups);
        }
        return getTotalFishAmount(fishGroups);
    }

    private void simulateTurn(long[] fishGroups) {
        long temp = fishGroups[0];
        System.arraycopy(fishGroups, 1, fishGroups, 0, fishGroups.length - 1);
        fishGroups[6] += temp; //Reset counter for the fish who just spawned offspring
        fishGroups[8] = temp; //Set counter for the fish who were just spawned
    }

    private long getTotalFishAmount(long[] fishGroups) {
        long sum = 0;
        for (long fishAmount : fishGroups) {
            sum += fishAmount;
        }
        return sum;
    }

    @Override
    public long part1() {
        return simulate(80, fishGroups.clone());
    }

    @Override
    public long part2() {
        return simulate(256, fishGroups.clone());
    }

    @Override
    public void setup() {
        fishGroups = new long[9];
        List<Integer> timerValues = ParsingUtils.getIntegers(lines.get(0));
        for (int timer : timerValues) {
            fishGroups[timer]++;
        }
    }
}
