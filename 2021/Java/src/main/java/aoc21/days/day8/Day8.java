package aoc21.days.day8;

import aoc.days.Day;

import java.util.*;

public class Day8 extends Day {
    private List<Entry> entries;

    @Override
    public void setup() {
        entries = new ArrayList<>();
        for (String line : lines) {
            String[] inputOutput = line.split("\\|");
            String[] input = inputOutput[0].trim().split(" ");
            String[] output = inputOutput[1].trim().split(" ");
            List<String> inputList = Arrays.stream(input).toList();
            List<String> outputList = Arrays.stream(output).toList();
            entries.add(new Entry(inputList, outputList));
        }
    }

    private long getNumberOfEasyOutputs() {
        long sum = 0;
        for (Entry entry : entries) {
            sum += entry.getNumberOfEasyOutputs();
        }
        return sum;
    }

    @Override
    public long part1() {
        return getNumberOfEasyOutputs();
    }



    @Override
    public long part2() {
        long sum = 0;
        for (Entry entry : entries) {
            sum += entry.getOutputValue();
        }
        return sum;
    }
}
