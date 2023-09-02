package aoc21.days.day3;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {

    private int[] getNumberOfOnesPerColumn(List<String> lines) {
        int numberOfColumns = lines.get(0).length();
        int[] numberOfOnes = new int[numberOfColumns];
        for (int i = 0; i < numberOfColumns; i++) {
            numberOfOnes[i] = getNumberOfOnes(lines, i);
        }
        return numberOfOnes;
    }

    private int getNumberOfOnes(List<String> lines, int column) {
        int count = 0;
        for (String line : lines) {
            if (line.charAt(column) == '1') {
                count++;
            }
        }
        return count;
    }

    private int[] calculateGammaRate(List<String> lines) {
        int[] counts = getNumberOfOnesPerColumn(lines);
        double threshHold = lines.size() / 2.0;
        int[] gammaRate = new int[lines.get(0).length()];
        for (int i = 0; i < counts.length; i++) {
            if (counts[i] > threshHold) {
                gammaRate[i] = 1;
            } else if (counts[i] < threshHold) {
                gammaRate[i] = 0;
            }
        }
        return gammaRate;
    }

    private long binaryToDecimal(int[] binary) {
        long result = 0;
        for (int bit : binary) {
            result *= 2;
            result += bit;
        }
        return result;
    }

    private long binaryToDecimal(String binary) {
        long result = 0;
        for (char bit : binary.toCharArray()) {
            result *= 2;
            result += bit - '0';
        }
        return result;
    }

    private String filter(List<String> lines, boolean searchMostCommon, int bitPosition) {
        if (lines.size() == 1) {
            return lines.get(0);
        }
        if (lines.size() < 1) {
            throw new IllegalArgumentException("All lines were filtered out.");
        }
        if (bitPosition < 0) {
            throw new IllegalArgumentException("All bit positions were considered and not enough numbers were filtered out.");
        }
        List<String> toKeep = new ArrayList<>();
        int numberOfOnes = getNumberOfOnes(lines, bitPosition);
        double threshold = lines.size() / 2.0;
        char charToKeep;
        if (searchMostCommon) {
            if (numberOfOnes >= threshold) {
                charToKeep = '1';
            } else {
                charToKeep = '0';
            }
        } else {
            if (numberOfOnes >= threshold) {
                charToKeep = '0';
            } else {
                charToKeep = '1';
            }
        }
        for (String line : lines) {
            if (line.charAt(bitPosition) == charToKeep) {
                toKeep.add(line);
            }
        }
        return filter(toKeep, searchMostCommon, bitPosition + 1);
    }

    @Override
    public long part1() {
        int numberLength = lines.get(0).length();
        int[] gammaRate = calculateGammaRate(lines);
        int[] epsilonRate = new int[numberLength];
        for (int i = 0; i < gammaRate.length; i++) {
            epsilonRate[i] = 1 - gammaRate[i];
        }
        long gammaRateDecimal = binaryToDecimal(gammaRate);
        long epsilonRateDecimal = binaryToDecimal(epsilonRate);
        return gammaRateDecimal * epsilonRateDecimal;
    }

    @Override
    public long part2() {
        String oxygenRating = filter(lines, true, 0);
        String scrubberRating = filter(lines, false, 0);
        long oxygenRatingDecimal = binaryToDecimal(oxygenRating);
        long scrubberRatingDecimal = binaryToDecimal(scrubberRating);
        return oxygenRatingDecimal * scrubberRatingDecimal;
    }

    @Override
    public void setup() {
    }
}
